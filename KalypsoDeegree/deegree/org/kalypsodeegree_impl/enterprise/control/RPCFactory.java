
package org.deegree_impl.enterprise.control;

import java.io.Reader;
import java.util.Date;

import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCFault;
import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCMethodResponse;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.TimeTools;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Factory class for creating RPC methodCall and methodResponse objects from
 * their XML representation
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class RPCFactory {
    
    /**
     * creates an instance of <tt>RPCMethodCall</tt> from an XML document that
     * can be accessed through the passed <tt>Reader</tt>
     *
     * @param reader reader to access an XML document
     * @return an RPCMethodCall
     */
    public static RPCMethodCall createRPCMethodCall(Reader reader) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createRPCMethodCall(Reader)" );
                
        Document doc = null;
        try {
            doc = XMLTools.parse( reader );
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return createRPCMethodCall( doc );
    }
    
    /**
     * creates an instance of <tt>RPCMethodCall</tt> from the XML document passed
     *
     * @param doc XML document containing a RPC method call
     * @return an RPCMethodCall
     */
    public static RPCMethodCall createRPCMethodCall(Document doc) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createRPCMethodCall(Document)" );
        
        RPCMethodCall mc = null;        
        try {
            Element root = doc.getDocumentElement();            
            // get methode name - mandatory
            String methodName = 
                XMLTools.getRequiredStringValue( "methodName",  null, root );
            
            Element params = XMLTools.getChildByName( "params", null, root );
                        
            RPCParameter[] parameters = null;
            if ( params != null ) {
                ElementList el = XMLTools.getChildElements( params );
                if ( el != null ) {
                    parameters = new RPCParameter[ el.getLength() ];
                    for (int i = 0; i < el.getLength(); i++) {
                        parameters[i] = createRPCParam( el.item( i ) );
                    }
                }
            }
            
            mc = new RPCMethodCall_Impl( methodName, parameters );
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return mc;
    }
    
    /**
     * creates an instance of <tt>RPCMethodResponse</tt> from an XML document that
     * can be accessed through the passed <tt>Reader</tt>
     *
     * @param reader reader to access an XML document
     * @return created <tt>RPCMethodResponse</tt>
     */
    public static RPCMethodResponse createRPCMethodResponse(Reader reader) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createRPCMethodResponse(Reader)" );
                
        Document doc = null;
        try {
            doc = XMLTools.parse( reader );
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return createRPCMethodResponse( doc );
    }
    
    /**
     * creates an instance of <tt>RPCMethodResponse</tt> from the XML document passed
     *
     * @param doc XML document containing a RPC method call
     * @return created <tt>RPCMethodResponse</tt>
     */
    public static RPCMethodResponse createRPCMethodResponse(Document doc) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createRPCMethodResponse(Document)" );
        
        RPCMethodResponse mc = null;        
        try {
            Element root = doc.getDocumentElement();            
                    
            Element params = XMLTools.getChildByName( "params", null, root );
            
            if ( params != null ) {
                ElementList el = XMLTools.getChildElements( params );
                RPCParameter[] parameters = null;
                if ( el != null ) {
                    parameters = new RPCParameter[ el.getLength() ];
                    for (int i = 0; i < el.getLength(); i++) {
                        parameters[i] = createRPCParam( el.item( i ) );
                    }
                }
                mc = new RPCMethodResponse_Impl( parameters );
            } else {
                // a fault is contained instead of the expected result
                Element fault = XMLTools.getChildByName( "fault", null, root );
                RPCFault rpcFault = createRPCFault( fault );
                mc = new RPCMethodResponse_Impl( rpcFault );
            }
            
            
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return mc;
    }
    
    public static RPCMethodResponse createRPCMethodResponse(RPCParameter[] par) throws RPCException {
       Debug.debugMethodBegin( "RPCFactory", "createRPCMethodResponse(RPCParameter[])" );
       RPCMethodResponse mc = null;  
       if ( par != null )   {
            RPCParameter[] params = par;             
            mc = new RPCMethodResponse_Impl( params );
        }
        else {
            System.out.println("Fehler createRPCMEthodResponse in RPSFactory");
       }
        return mc;
    }
    
    /**
     * creates a <tt>RPCParameter</tt> from its XML representation
     *
     * @param param element containing a RPC param
     * @return created <tt>RPCParameter</tt>
     */
    private static RPCParameter createRPCParam(Element param) throws RPCException {
        Debug.debugMethodBegin();
        
        RPCParameter parameter = null;
        try {
            Element value = XMLTools.getChildByName( "value", null, param );
            Element child = XMLTools.getFirstElement( value );
            Object o = null;
            Class cl = null;
            if ( child.getNodeName().equals( "struct" ) ) {
                o = createRPCStruct( child );
                cl = RPCStruct.class;
            } else if ( child.getNodeName().equals( "string" ) ) {
                o = XMLTools.getRequiredStringValue( "string", null,  value );
                cl = String.class;
            } else if ( child.getNodeName().equals( "int" ) ) {
                double d = XMLTools.getRequiredDoubleValue( "int", null,  value );
                o = new Integer( (int)d );
                cl = Integer.class;
            } else if ( child.getNodeName().equals( "i4" ) ) {
                double d = XMLTools.getRequiredDoubleValue( "int", null,  value );
                o = new Integer( (int)d );
                cl = Integer.class;
            } else if ( child.getNodeName().equals( "double" ) ) {
                double d = XMLTools.getRequiredDoubleValue( "double", null,  value );
                o = new Double( d );
                cl = Double.class;
            } else if ( child.getNodeName().equals( "boolean" ) ) {
                o = new Boolean( child.getFirstChild().getNodeValue().equals("1") );
                cl = Boolean.class;
            } else if ( child.getNodeName().equals( "dateTime.iso8601" ) ) {
                String s = XMLTools.getRequiredStringValue( "dateTime.iso8601", null,  value );
                o = TimeTools.createCalendarISO8601( s ).getTime();
                cl = Date.class;
            } else if ( child.getNodeName().equals( "base64" ) ) {
            } else if ( child.getNodeName().equals( "array" ) ) {
                o = createArray( child );
                cl = RPCParameter[].class;
            }
            parameter = new RPCParameter_Impl( cl, o );
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return parameter;
    }
    
    /**
     * creates a RPC struture object from the passed <tt>Element</tt>
     *
     * @param struct element containing a RPC struct
     * @return created <tt>RPCStruct</tt>
     */
    private static RPCStruct createRPCStruct(Element struct) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createRPCStruct" );
        
        RPCStruct structure = null;
        try {
            ElementList el = XMLTools.getChildElements( struct );
            RPCMember[] members = null;
            if ( el != null ) {
                members = new RPCMember[ el.getLength() ];
                for (int i = 0; i < el.getLength(); i++) {
                    members[i] = createRPCMember( el.item(i) );
                }
            }
            structure = new RPCStruct_Impl( members );
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return structure;
    }
    
    /**
     * creates a RPC struture member object from the passed <tt>Element</tt>
     *
     * @param member element containing a RPC member
     * @return created <tt>RPCMember</tt>
     */
    private static RPCMember createRPCMember(Element member) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createRPCMember" );
 
        RPCMember mem = null;
        try {
            String name = XMLTools.getRequiredStringValue( "name", null,  member );
            Element value = XMLTools.getChildByName( "value", null, member );
            Element child = XMLTools.getFirstElement( value );  
            Object o = null;
            Class cl = null;
            if ( child.getNodeName().equals( "struct" ) ) {
                o = createRPCStruct( child );
                cl = RPCStruct.class;
            } else if ( child.getNodeName().equals( "string" ) ) {
                o = XMLTools.getRequiredStringValue( "string", null,  value );
                cl = String.class;
            } else if ( child.getNodeName().equals( "int" ) ) {
                double d = XMLTools.getRequiredDoubleValue( "int", null,  value );
                o = new Integer( (int)d );
                cl = Integer.class;
            } else if ( child.getNodeName().equals( "i4" ) ) {
                double d = XMLTools.getRequiredDoubleValue( "int", null,  value );
                o = new Integer( (int)d );
                cl = Integer.class;
            } else if ( child.getNodeName().equals( "double" ) ) {
                double d = XMLTools.getRequiredDoubleValue( "double", null,  value );
                o = new Double( d );
                cl = Double.class;
            } else if ( child.getNodeName().equals( "boolean" ) ) {
                o = new Boolean( child.getFirstChild().getNodeValue().equals("1") );
                cl = Boolean.class;
            } else if ( child.getNodeName().equals( "dateTime.iso8601" ) ) {
                String s = XMLTools.getRequiredStringValue( "dateTime.iso8601", null,  value );
                o = TimeTools.createCalendarISO8601( s ).getTime();
                cl = Date.class;
            } else if ( child.getNodeName().equals( "base64" ) ) {
            } else if ( child.getNodeName().equals( "array" ) ) {
                o = createArray( child );
                cl = RPCParameter[].class;
            }
            mem = new RPCMember_Impl( cl, o, name );
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return mem;
    }
    
    /**
     * creates an <tt>ArrayList</tt> object from the passed <tt>Element</tt>
     *
     * @param array 
     * @return created <tt>ArrayList</tt>
     */
    private static RPCParameter[] createArray(Element array) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createArray" );
        
        RPCParameter[] param = null;
        try {
            Element data = XMLTools.getChildByName( "data",  null, array );
            ElementList el = XMLTools.getChildElements( data );
            if ( el != null ) {
                param = new RPCParameter[el.getLength()];
                for (int i = 0; i < el.getLength(); i++) {
                    Element child = XMLTools.getFirstElement( el.item(i) );
                    Object o = null;
                    Class cl = null;
                    if ( child.getNodeName().equals( "struct" ) ) {
                        o = createRPCStruct( child );
                        cl = RPCStruct.class;
                    } else if ( child.getNodeName().equals( "string" ) ) {
                        o = XMLTools.getRequiredStringValue( "string", null,  el.item(i) );
                        cl = String.class;
                    } else if ( child.getNodeName().equals( "int" ) ) {
                        double d = XMLTools.getRequiredDoubleValue( "int", null,  el.item(i) );
                        o = new Integer( (int)d );
                        cl = Integer.class;
                    } else if ( child.getNodeName().equals( "i4" ) ) {
                        double d = XMLTools.getRequiredDoubleValue( "int", null,  el.item(i) );
                        o = new Integer( (int)d );
                        cl = Integer.class;
                    } else if ( child.getNodeName().equals( "double" ) ) {
                        double d = XMLTools.getRequiredDoubleValue( "double", null,  el.item(i) );
                        o = new Double( d );
                        cl = Double.class;
                    } else if ( child.getNodeName().equals( "boolean" ) ) {
                        o = new Boolean( child.getFirstChild().getNodeValue().equals("1") );
                        cl = Boolean.class;
                    } else if ( child.getNodeName().equals( "dateTime.iso8601" ) ) {
                        String s = XMLTools.getRequiredStringValue( "dateTime.iso8601", null,  el.item(i));
                        o = TimeTools.createCalendarISO8601( s ).getTime();
                        cl = Date.class;
                    } else if ( child.getNodeName().equals( "base64" ) ) {
                    } else if ( child.getNodeName().equals( "array" ) ) {
                        o = createArray( child );
                        cl = RPCParameter[].class;
                    }
                    param[i] = new RPCParameter_Impl( cl, o );
                }
            }
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return param;
    }
    
    /**
     * creates an <tt>RPCFault</tt> object from the passed <tt>Element</tt>
     *
     * @param fault fault element
     * @return created <tt>RPCFault</tt>
     */
    private static RPCFault createRPCFault( Element fault ) throws RPCException {
        Debug.debugMethodBegin( "RPCFactory", "createRPCFault" );
        
        RPCFault rpcFault = null;
        try {
            Element value = XMLTools.getChildByName( "value", null, fault );
            Element child = XMLTools.getFirstElement( value );  
            RPCStruct struct = createRPCStruct( child );          
            String s1 = null;
            String s2 = null;
            Object o = struct.getMember( "faultCode" ).getValue();
            if ( o != null ) {
                s1 = o.toString();
            }
            o = struct.getMember( "faultString" ).getValue();
            if ( o != null ) {
                s2 = o.toString();
            }
            rpcFault = new RPCFault_Impl( s1, s2 );
        } catch (Exception e) {
            Debug.debugException( e, "" );
            throw new RPCException( e.toString() );
        }
        
        Debug.debugMethodEnd();
        return rpcFault;
    }    
    
}
