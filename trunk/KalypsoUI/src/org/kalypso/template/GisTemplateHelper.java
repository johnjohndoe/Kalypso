package org.kalypso.template;

import java.io.OutputStream;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.gistableview.Gistableview;
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

  public static Gistableview loadGisTableview( final IFile file ) throws CoreException, JAXBException
  {
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );

    return (Gistableview)new org.kalypso.template.gistableview.ObjectFactory().createUnmarshaller().unmarshal( is );
  }

  public static void saveGisMapView( Gismapview modellTemplate,OutputStream outStream ) throws JAXBException
  {
    
    // TODO: create marschaller only once
    
    new ObjectFactory().createMarshaller().marshal( modellTemplate,outStream);  
  }
}
