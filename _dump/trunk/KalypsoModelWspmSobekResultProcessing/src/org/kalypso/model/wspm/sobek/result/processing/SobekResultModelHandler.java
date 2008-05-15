package org.kalypso.model.wspm.sobek.result.processing;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

public class SobekResultModelHandler implements ISobekResultModel
{
  private final IFolder m_resultFolder;

  private SobekResultModelHandler( final IFolder resultFolder )
  {
    m_resultFolder = resultFolder;
  }

  /**
   * @param resultFolder
   *            Sobek result folder
   * @return Sobek Result Model
   */
  public static ISobekResultModel getHandler( final IFolder resultFolder ) throws CoreException
  {
    if( !resultFolder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( "Sobek result folder doesn't exists" ) );

    return new SobekResultModelHandler( resultFolder );
  }

  public TimeSeriesComplexType getCrossSectionTimeSeries( final String crossSectionNodeId ) throws CoreException
  {
    final IFolder folder = m_resultFolder.getFolder( "output/nodes/csn/" );
    if( !folder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( "Sobek cross section node result folder doesn't exists" ) );

    final IFile file = folder.getFile( crossSectionNodeId + ".xml" );
    if( !file.exists() )
      return null;

    final InputStream is = new BufferedInputStream( file.getContents() );
    try
    {
      final JAXBContext jc = JAXBContext.newInstance( nl.wldelft.fews.pi.ObjectFactory.class );
      final Unmarshaller u = jc.createUnmarshaller();
      final JAXBElement<TimeSeriesComplexType> jaxRoot = (JAXBElement<TimeSeriesComplexType>) u.unmarshal( is );

      return jaxRoot.getValue();
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( "Reading Sobek Cross Section Node data file - failed." ) );
    }
    finally
    {
      try
      {
        is.close();
      }
      catch( final IOException e )
      {
        throw new CoreException( StatusUtilities.createWarningStatus( "Error closing Sobek Cross Section Node input stream." ) );
      }
    }
  }
}
