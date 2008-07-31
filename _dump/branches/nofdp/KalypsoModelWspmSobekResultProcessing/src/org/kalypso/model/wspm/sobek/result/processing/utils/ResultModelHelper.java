package org.kalypso.model.wspm.sobek.result.processing.utils;

import java.io.InputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;

public class ResultModelHelper
{

  public static IFile getNodeResultFile( IFolder iResultFolder, final INode node ) throws CoreException
  {
    final IFile iFile = iResultFolder.getFile( node.getId() + ".gml" ); //$NON-NLS-1$
    if( !iFile.exists() )
      return null;

    return iFile;
  }

  public static IFile getPiCalculationPointFile( IFolder resultFolder ) throws CoreException
  {
    final IFile iFile = resultFolder.getFile( "calcpnt.xml" ); //$NON-NLS-1$
    if( !iFile.exists() )
      return null;

    return iFile;
  }

  public static IFile getBranchHydrogrographWorkspaceFile( IFolder folder ) throws CoreException
  {
    IFile iBase = folder.getFile( "hydrograhps.gml" ); //$NON-NLS-1$

    if( !iBase.exists() )
    {
      final InputStream stream = ResultModelHelper.class.getResourceAsStream( "templates/templateSobekBranchHydrograph.gml" ); //$NON-NLS-1$
      iBase.create( stream, true, new NullProgressMonitor() );
    }

    return iBase;
  }
}
