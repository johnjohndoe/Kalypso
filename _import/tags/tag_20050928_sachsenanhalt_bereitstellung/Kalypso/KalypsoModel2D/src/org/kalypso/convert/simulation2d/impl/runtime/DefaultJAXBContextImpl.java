//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v1.0.3-b18-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2004.09.14 at 10:26:16 CEST 
//

package org.kalypso.convert.simulation2d.impl.runtime;

import javax.xml.bind.DatatypeConverter;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.PropertyException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Validator;

import com.sun.xml.bind.DatatypeConverterImpl;
import com.sun.xml.bind.Messages;

/**
 * This class provides the default implementation of JAXBContext.  It
 * also creates the GrammarInfoFacade that unifies all of the grammar
 * info from packages on the contextPath.
 *
 * @version $Revision$
 */
public class DefaultJAXBContextImpl extends JAXBContext {
    
    /**
     * This object keeps information about the grammar.
     * 
     * When more than one package are specified,
     * GrammarInfoFacade is used.
     */
    private GrammarInfo gi = null;

    /**
     * This is the constructor used by javax.xml.bind.FactoryFinder which
     * bootstraps the RI.  It causes the construction of a JAXBContext that
     * contains a GrammarInfoFacade which is the union of all the generated
     * JAXBContextImpl objects on the contextPath.
     */
    public DefaultJAXBContextImpl( String contextPath, ClassLoader classLoader ) 
        throws JAXBException {
            
        this( GrammarInfoFacade.createGrammarInfoFacade( contextPath, classLoader ) );

        // initialize datatype converter with ours
        DatatypeConverter.setDatatypeConverter(DatatypeConverterImpl.theInstance);
    }
    
    /**
     * This constructor is used by the default no-arg constructor in the
     * generated JAXBContextImpl objects.  It is also used by the 
     * bootstrapping constructor in this class.
     */
    public DefaultJAXBContextImpl( GrammarInfo gi ) {
        this.gi = gi;
    }
        
    public GrammarInfo getGrammarInfo() { 
        return gi;
    }
    
    
    
    /**
     * Once we load a grammar, we will cache the value here.
     */
    private com.sun.msv.grammar.Grammar grammar = null;
    
    /**
     * Loads a grammar object for the unmarshal-time validation.
     */
    public com.sun.msv.grammar.Grammar getGrammar() throws JAXBException {
        if( grammar==null )
            // since JAXBContext needs to be thread-safe,
            // this check might allow more than one thread to
            // load a grammar, but that's fine; it's just a loss of
            // time, but by itself it doesn't cause any problem.
            // by not synchronizing method, we can avoid its overhead.
            grammar = gi.getGrammar();
        return grammar;
    }
    
    
    /**
     * Create a <CODE>Marshaller</CODE> object that can be used to convert a
     * java content-tree into XML data.
     *
     * @return a <CODE>Marshaller</CODE> object
     * @throws JAXBException if an error was encountered while creating the
     *                      <code>Marshaller</code> object
     */
    public Marshaller createMarshaller() throws JAXBException {
            return new MarshallerImpl( this );
    }
       
    /**
     * Create an <CODE>Unmarshaller</CODE> object that can be used to convert XML
     * data into a java content-tree.
     *
     * @return an <CODE>Unmarshaller</CODE> object
     * @throws JAXBException if an error was encountered while creating the
     *                      <code>Unmarshaller</code> object
     */
    public Unmarshaller createUnmarshaller() throws JAXBException {
            return new UnmarshallerImpl( this, gi );
    }    
        
    /**
     * Create a <CODE>Validator</CODE> object that can be used to validate a
     * java content-tree.
     *
     * @return an <CODE>Unmarshaller</CODE> object
     * @throws JAXBException if an error was encountered while creating the
     *                      <code>Validator</code> object
     */
    public Validator createValidator() throws JAXBException {
            return new ValidatorImpl( this );
    }
    

    
    /**
     * Create an instance of the specified Java content interface.  
     *
     * @param javaContentInterface the Class object 
     * @return an instance of the Java content interface
     * @exception JAXBException
     */
    public Object newInstance( Class javaContentInterface ) 
        throws JAXBException {

        if( javaContentInterface == null ) {
            throw new JAXBException( Messages.format( Messages.CI_NOT_NULL ) );
        }

        try {
            Class c = gi.getDefaultImplementation( javaContentInterface );
            if(c==null)
                throw new JAXBException(
                    Messages.format( Messages.MISSING_INTERFACE, javaContentInterface ));
            
            return c.newInstance();
        } catch( Exception e ) {
            throw new JAXBException( e );
        } 
    }
    
    /**
     * There are no required properties, so simply throw an exception.  Other
     * providers may have support for properties on Validator, but the RI doesn't
     */
    public void setProperty( String name, Object value )
        throws PropertyException {
        
        throw new PropertyException(name, value);
    }
    
    /**
     * There are no required properties, so simply throw an exception.  Other
     * providers may have support for properties on Validator, but the RI doesn't
     */
    public Object getProperty( String name )
        throws PropertyException {
            
        throw new PropertyException(name);
    }
    
    
}
