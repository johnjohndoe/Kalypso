package org.kalypso.model.transformation;

import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
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
    final String input = properties.getProperty( "input" );
    final String xsl = properties.getProperty( "xsl" );
    final String output = properties.getProperty( "output" );

    final IProject project = targetFolder.getProject();

    final IFile inputFile = (IFile)project.findMember( input );
    final IFile xslFile = (IFile)project.findMember( xsl );
    final IFile outputFile = targetFolder.getFile( output );

    System.out.println( "input exists:" + inputFile.exists() );
    System.out.println( "xsl exists:" + xslFile.exists() );

    try
    {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware( true );
      DocumentBuilder docuBuilder = factory.newDocumentBuilder();
      Document xmlDOM = docuBuilder.parse( inputFile.getContents() );
      Document xslDOM = docuBuilder.parse( xslFile.getContents() );
      TransformerFactory transformerFactory = TransformerFactory.newInstance();
      //    transformerFactory.setAttribute("version",new String("1.0"));
      Transformer transformer = transformerFactory.newTransformer( new DOMSource( xslDOM ) );

      PipedOutputStream outStream = new PipedOutputStream();
      outputFile.create( new PipedInputStream( outStream ), true, new NullProgressMonitor() );
      transformer.transform( new DOMSource( xmlDOM ), new StreamResult( outStream ) );
    }
    catch( CoreException e )
    {
      throw new TransformationException( e );
    }
    catch( TransformerException e )
    {
      throw new TransformationException( e );
    }
    catch( Exception e )
    {
      throw new TransformationException( e );
    }

  }

}