package org.kalypso.util.transformation;

import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.java.lang.CatchThread;
import org.w3c.dom.Document;

/**
 * @author belger
 */
public class XslTransformation extends AbstractTransformation
{
  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final Properties properties, final IProgressMonitor monitor )
      throws TransformationException
  {
    monitor.beginTask( "XSL Transformation", 3000 );
    
    final String input = properties.getProperty( "input" );
    final String xsl = properties.getProperty( "xsl" );
    final String output = properties.getProperty( "output" );

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    
    final IFile inputFile = (IFile)root.findMember( input );
    final IFile xslFile = (IFile)root.findMember( xsl );
    final IFile outputFile = (IFile)root.findMember( output );

    try
    {
      final StreamSource xslSource = new StreamSource( xslFile.getContents(), xslFile.getCharset() );  
      
      final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware( true );
      final DocumentBuilder docuBuilder = factory.newDocumentBuilder();
      final Document xmlDOM = docuBuilder.parse( inputFile.getContents() );
      final TransformerFactory transformerFactory = TransformerFactory.newInstance();
      final Transformer transformer = transformerFactory.newTransformer( xslSource );
      
      final PipedInputStream pis = new PipedInputStream();
      final PipedOutputStream pos = new PipedOutputStream( pis );
      final CatchThread thread = new CatchThread()
      {
        protected void runIntern() throws Throwable
        {
          transformer.transform( new DOMSource( xmlDOM ), new StreamResult( pos ) );
          pos.close();
        }
      };
      thread.start();
      
      monitor.worked( 1000 );
      
      outputFile.create( pis, false, new SubProgressMonitor( monitor, 1000 ) );
      outputFile.setCharset( inputFile.getCharset(), new SubProgressMonitor( monitor, 1000 ) );

      if( thread.getThrown() != null )
        throw new TransformationException( thread.getThrown() );
    }
    catch( Exception e )
    {
      throw new TransformationException( e );
    }
  }

}