
package org.deegree_impl.enterprise.control;

import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

import org.deegree.enterprise.control.*;
import org.deegree_impl.tools.Debug;


/**
 *
 * @author  Administrator
 */
public class RPCWebEvent extends WebEvent {
    
    private RPCMethodCall mc = null;
    
    /** Creates a new instance of RPCWebEvent */
    public RPCWebEvent(HttpServletRequest request) {
        super( request );
    }
    
    /** Creates a new instance of RPCWebEvent */
    public RPCWebEvent(HttpServletRequest request, RPCMethodCall mc) {
        super( request );
        this.mc = mc;
    }
    
    /** Creates a new instance of RPCWebEvent */
    public RPCWebEvent(FormEvent parent, RPCMethodCall mc) {
        super( (HttpServletRequest)parent.getSource() );
        this.mc = mc;
    }
    
    /**
     * returns the the RPC methodcall extracted from the <tt>HttpServletRequest</tt>
     * passed to the first constructor.
     */
    public RPCMethodCall getRPCMethodCall() {
        if ( mc == null ) {
            try {
                mc = getMethodCall( (ServletRequest)this.getSource() );
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return mc;
    }
    
    /**
     * extracts the RPC method call from the 
     */
    private RPCMethodCall getMethodCall(ServletRequest request ) throws RPCException {
        Debug.debugMethodBegin( this, "getMethodCall" );
        
        StringBuffer sb = new StringBuffer(1000);
        try {
            BufferedReader br = request.getReader();
            String line = null;
            while ( (line = br.readLine() ) != null ) {
                sb.append( line );
            }
            br.close();
        } catch (Exception e) {
            throw new RPCException( "Error reading stream from servlet\n" + e.toString() );
        }
        
        String s = sb.toString();
        int pos1 = s.indexOf( "<methodCall>" );
        int pos2 = s.indexOf( "</methodCall>" );
        if ( pos1 < 0 ) {
            throw new RPCException( "request doesn't contain a RPC methodCall" );
        }
        s = s.substring( pos1, pos2 + 13 );
        
        StringReader reader = new StringReader( s );
        RPCMethodCall mc = RPCFactory.createRPCMethodCall( reader );
        
        Debug.debugMethodEnd();
        return mc;
    }
}
