/*
 * Created on 24.03.2004
 */
package de.psi.go.lhwz;

/**
 * @author ikunin
 */
public class ECommException extends Exception{
    
    /**
     *
     */
    public ECommException() {
        super();
    }
    /**
     * @param message
     */
    public ECommException(String message) {
        super(message);
    }
    /**
     * @param message
     * @param cause
     */
    public ECommException(String message, Throwable cause) {
        super(message, cause);
    }
    /**
     * @param cause
     */
    public ECommException(Throwable cause) {
        super(cause);
    }
}
