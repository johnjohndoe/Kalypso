package org.kalypso.ui.nature;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;


public class CalcCaseCollector implements IResourceVisitor
{
  private Collection m_calcCases = new ArrayList();

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( final IResource resource )
  {
    if( resource.getType() == IResource.FOLDER
        && ModelNature.isCalcCalseFolder( (IFolder)resource ) )
    {
      m_calcCases.add( resource );
      return false;
    }

    return true;
  }

  public IFolder[] getCalcCases()
  {
    return (IFolder[])m_calcCases.toArray( new IFolder[m_calcCases.size()] );
  }
}