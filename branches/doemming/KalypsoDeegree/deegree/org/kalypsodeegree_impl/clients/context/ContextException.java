/*
 * CatalogClientException.java
 *
 * Created on 6. November 2003, 22:15
 */

package org.deegree_impl.clients.context;

import org.deegree_impl.tools.StringExtend;

/**
 *
 * @author  Administrator
 */
public class ContextException extends java.lang.Exception {
        
    private String st = "";
    
    /**
     * Creates a new instance of <code>CatalogClientException</code> without detail message.
     */
    public ContextException() {
    	st = "ContextException";
    }
    
     
    /**
     * Constructs an instance of <code>CatalogClientException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public ContextException(String msg) {
        super( msg );
    }
       
    /**
     * Constructs an instance of <code>CatalogClientException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public ContextException(String msg, Exception e) {
        this( msg );
        st = StringExtend.stackTraceToString( e.getStackTrace() );
    }
    
    public String toString() {
        return super.toString() + "\n" + st;
    }
    
}
