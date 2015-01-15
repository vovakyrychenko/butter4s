/*
 * The MIT License
 *
 * Copyright (c) 2010 Vladimir Kirichenko <vladimir.kirichenko@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package butter4s.net.http.rest

import butter4s.lang.reflect._
import butter4s.io._
import butter4s.json.Binder
import butter4s.logging.Logging
import java.lang.reflect.InvocationTargetException
import java.lang.RuntimeException

/**
 * @author Vladimir Kirichenko <vladimir.kirichenko@gmail.com> 
 */

class ImmediateResponse(val code: Int, val message: String) extends RuntimeException(message)

object Service {
    private[rest] var producers = Map[String, Any => String](
        MimeType.TEXT_JAVASCRIPT -> (content => String.valueOf(content)),
        MimeType.TEXT_PLAIN -> (content => String.valueOf(content)),
        MimeType.APPLICATION_JSON -> (content => Binder.marshal(content))
    )

    def registerContentProducer(contentType: String, produce: Any => String) = producers += contentType -> produce

    private var converters = Map[raw.Type[_], (String, parameterized.Type[_]) => Any](
        typeOf[Int].rawType -> ((s, _) => s.toInt),
        typeOf[java.lang.Integer].rawType -> ((s, _) => s.toInt),
        typeOf[Long].rawType -> ((s, _) => s.toLong),
        typeOf[java.lang.Long].rawType -> ((s, _) => s.toLong),
        typeOf[Short].rawType -> ((s, _) => s.toShort),
        typeOf[java.lang.Short].rawType -> ((s, _) => s.toShort),
        typeOf[Byte].rawType -> ((s, _) => s.toByte),
        typeOf[java.lang.Byte].rawType -> ((s, _) => s.toByte),
        typeOf[Float].rawType -> ((s, _) => s.toFloat),
        typeOf[java.lang.Float].rawType -> ((s, _) => s.toFloat),
        typeOf[Double].rawType -> ((s, _) => s.toDouble),
        typeOf[java.lang.Double].rawType -> ((s, _) => s.toDouble),
        typeOf[Boolean].rawType -> ((s, _) => s.toBoolean),
        typeOf[java.lang.Boolean].rawType -> ((s, _) => s.toBoolean),
        typeOf[String].rawType -> ((s, _) => s),
        typeOf[Char].rawType -> ((s, _) => if (s == null) (0: Char) else s(0)),
        typeOf[Symbol].rawType -> ((s, _) => Symbol(s)),
        typeOf[Enum[_]].rawType -> ((s, targetType) => targetType.as[parameterized.EnumType].rawType.valueOf(s))
    )

    private var binders = Map[String, (String, parameterized.Type[_]) => Any](
        MimeType.APPLICATION_JSON -> ((s, t) => Binder.unmarshal(s, t).get)
    )

    def convert(value: String, hint: String, targetType: parameterized.Type[_]) =
        (if (hint == MimeType.APPLICATION_JAVA_CLASS) converters.find {
            case (t, _) => t <:< targetType.rawType
        } match {
            case Some(x) => x._2
            case None => throw new RuntimeException("undefined converter for type " + targetType)
        }
        else binders(hint))(value, targetType)

    def registerParameterBinder(typeHint: String, convert: (String, parameterized.Type[_]) => Any) = binders += typeHint -> convert

    private[rest] var apis = Map[String, (Request, parameterized.ClassType[AnyRef], Boolean) => String]("prototype" -> PrototypeApi, "dojo" -> DojoApi)
}

trait Service extends Logging {

    import Response.Code._

    def perform(request: Request, response: Response) = log.time(request.toString, try {
        log.debug("invoke " + request)
        parameterized.Type.fromClass(getClass.asInstanceOf[Class[Service]]).as[parameterized.ClassType].methods.find(Request.methodMatches(request.requestLine, request.httpMethod, _)) match {
            case None => respond(NOT_FOUND, request.requestLine + " is unbound")
            case Some(method) => method.raw.annotation[Method] match {
                case None => respond(NOT_FOUND, method.raw.name + " is not exposed")
                case Some(restMethod) =>
                    val result = method.raw.invoke(this, method.parameters.map(p => {
                        log.debug(p)
                        if (p.raw.rawType <:< typeOf[Request].rawType) request
                        else if (p.raw.rawType <:< typeOf[List[_]].rawType) p.raw.annotation[Param] match {
                            case None => respond(INTERNAL_SERVER_ERROR, "method parameter of type " + p.raw.rawType + " is not annotated properly")
                            case Some(restParam) => request.parameters(restParam.name).map(Service.convert(_, restParam.typeHint, p.actualType.arguments(0)))
                        } else if (p.raw.rawType <:< typeOf[Option[_]].rawType) p.raw.annotation[Param] match {
                            case None => respond(INTERNAL_SERVER_ERROR, "method parameter of type " + p.raw.rawType + " is not annotated properly")
                            case Some(restParam) => request.parameter(restParam.name).map(Service.convert(_, restParam.typeHint, p.actualType.arguments(0)))
                        } else p.raw.annotation[Param] match {
                            case None => respond(INTERNAL_SERVER_ERROR, "method parameter of type " + p.raw.rawType + " is not annotated properly")
                            case Some(restParam) => Service.convert((restParam.from match {
                                case Param.From.BODY => Some(request.body.readAs[String])
                                case Param.From.QUERY => request.parameter(restParam.name)
                                case Param.From.PATH => request.parameter(restMethod.path, restParam.name)
                            }) match {
                                case None => respond(BAD_REQUEST, restParam.name + " is required")
                                case Some(value) => value
                            }, restParam.typeHint, p.actualType).asInstanceOf[AnyRef]
                        }
                    }): _ *)

                    if (restMethod.raw) response.content(restMethod.produces + "; charset=" + restMethod.charset, _.write(String.valueOf(result)))
                    else Service.producers.get(restMethod.produces) match {
                        case None => response.status(if (result == null) OK else result.asInstanceOf[Int])
                        case Some(toContent) => response.content(restMethod.produces + "; charset=" + restMethod.charset, _.write(toContent(result)))
                    }
            }
        }
    } catch {
        case e: ImmediateResponse =>
            log.error(e, e)
            response.status(e.code, e.message)
        case e: InvocationTargetException if e.getTargetException.isInstanceOf[ImmediateResponse] =>
            log.error(e.getTargetException, e.getTargetException)
            response.status(e.getTargetException.asInstanceOf[ImmediateResponse].code, e.getTargetException.getMessage)
        case e: InvocationTargetException =>
            log.error(e, e)
            throw e.getTargetException
        case e: Throwable =>
            log.error(e, e)
            throw e
    })

    @Method(produces = "text/plain", info = "this information")
    def _info(request: Request) = parameterized.Type.fromClass(getClass).as[parameterized.ClassType].methods.view.filter(m => m.raw.annotatedWith[Method]).map {
        method => {
            val restMethod = method.raw.annotation[Method].get
            val params = method.parameters.view.filter(_.raw.annotatedWith[Param])
            "method:\t" + method.raw.name + "\n" +
                    "info:\t" + restMethod.info + "\n" +
                    "path:\t" + request.context.serviceLocation + (if (restMethod.path == Method.Constants.DEFAULT) "/" + method.raw.name else restMethod.path) + "\n" +
                    "http:\t" + restMethod.httpMethod.mkString(",") + "\n" +
                    "query-params:\n" + params.filter(_.raw.annotation[Param].get.from == Param.From.QUERY).map {
                param => {
                    val restParam = param.raw.annotation[Param].get
                    "\t" + restParam.name + " : " + param.actualType.toTypeString + "\n"
                }
            }.mkString +
                    "body:\t" + params.find(_.raw.annotation[Param].get.from == Param.From.BODY).map(_.actualType.toTypeString).orElse(Some("none")).get
        }
    }.mkString("\n\n\n") + "\n"

    @Method(produces = "text/javascript", info = "JavaScript API")
    def _api(request: Request, @Param(name = "for") name: Option[String], @Param(name = "qualified") qualified: Option[java.lang.Boolean]) = {
        val apiFor = name.getOrElse("prototype")
        Service.apis.get(apiFor) match {
            case Some(api) => api(request, parameterized.Type.fromClass(getClass.asInstanceOf[Class[AnyRef]]).as[parameterized.ClassType], qualified.isDefined && qualified.get.asInstanceOf[Boolean])
            case _ => respond(NOT_FOUND, "unknown API " + apiFor)
        }
    }

    def respond(code: Int, reason: String) = throw new ImmediateResponse(code, reason)

}


