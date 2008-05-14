package org.kalypso.model.wspm.sobek.result.processing;

import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

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

    throw new NotImplementedException();
  }

}
