package org.kalypso.template;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
import org.xml.sax.InputSource;


/**
 * Hilfsklasse, um aus den Binding-Klassen 'echte' Objekte zu erzeugen und umgekehrt
 * 
 * @author Belger
 */
public class GisTemplateHelper
{
  private GisTemplateHelper()
  {
    // never instantiate this class
  }
  
  public static final Gismapview loadGisMapView( final IFile file ) throws JAXBException, CoreException
  {
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );
    
    // TODO: create unmarschaller only once
    
    return (Gismapview)new ObjectFactory().createUnmarshaller().unmarshal( is );
  }
}
