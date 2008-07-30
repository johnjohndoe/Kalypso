package org.kalypso.model.wspm.sobek.result.processing.utils;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
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

  public static IFile getResultFile( IFolder resultFolder ) throws CoreException
  {
    final IFile iFile = resultFolder.getFile( "calcpnt.xml" ); //$NON-NLS-1$
    if( !iFile.exists() )
      return null;

    return iFile;
  }
}
