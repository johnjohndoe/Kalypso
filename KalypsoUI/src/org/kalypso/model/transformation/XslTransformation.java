package org.kalypso.model.transformation;

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
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
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
   * @throws TransformationException
   * @see org.kalypso.model.transformation.ICalculationCaseTransformation#transform(org.eclipse.core.resources.IFolder,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final IFolder targetFolder, final Properties properties, final IProgressMonitor monitor )
      throws TransformationException
  {
    monitor.beginTask( "XSL Transformation", 3000 );
    
    final String input = properties.getProperty( "input" );
    final String xsl = properties.getProperty( "xsl" );
    final String output = properties.getProperty( "output" );

    final IProject project = targetFolder.getProject();

    final IFile inputFile = (IFile)project.findMember( input );
    final IFile xslFile = (IFile)project.findMember( xsl );
    final IFile outputFile = targetFolder.getFile( output );

    try
    {
      final StreamSource xslSource = new StreamSource( xslFile.getContents(), xslFile.getCharset() );  
      
      final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware( true );
      final DocumentBuilder docuBuilder = factory.newDocumentBuilder();
      final Document xmlDOM = docuBuilder.parse( inputFile.getContents() );
      //final Document xslDOM = docuBuilder.parse( xslFile.getContents() );
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
      
      if( thread.getThrowable() != null )
        throw new TransformationException( thread.getThrowable() );
    }
    catch( Exception e )
    {
      throw new TransformationException( e );
    }
  }

}