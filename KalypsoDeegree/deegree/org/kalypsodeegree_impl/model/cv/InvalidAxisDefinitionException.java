/*
 * InvalidAxisDefinitionException.java
 *
 * Created on 4. November 2002, 20:50
 */

package org.deegree_impl.model.cv;

/**
 *
 * @author  Administrator
 */
public class InvalidAxisDefinitionException extends Exception {
    
    /**
     * Creates a new instance of <code>InvalidAxisDefinitionException</code> without detail message.
     */
    public InvalidAxisDefinitionException() {
    }
    
    
    /**
     * Constructs an instance of <code>InvalidAxisDefinitionException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public InvalidAxisDefinitionException(String msg) {
        super(msg);
    }
}
