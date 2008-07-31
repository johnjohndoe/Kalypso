package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.result.processing.SobekResultModelHandler;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrograph;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrographModel;
import org.kalypso.model.wspm.sobek.result.processing.utils.BranchHydrographUtilities;
import org.kalypso.model.wspm.sobek.result.processing.utils.ResultModelHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public class BranchHydraphModelHandler extends AbstractListWrapper implements IBranchHydrographModel
{
  private final CommandableWorkspace m_workspace;

  private final SobekResultModelHandler m_resultModelHandler;

  public BranchHydraphModelHandler( final SobekResultModelHandler resultModelHandler, final CommandableWorkspace workspace, final FeatureList list )
  {
    super( list );
    m_resultModelHandler = resultModelHandler;

    m_workspace = workspace;
  }

  public IBranchHydrograph getHydrograph( final IBranch branch ) throws CoreException
  {
    try
    {
      final IBranchHydrograph[] hydrographs = getHydrographs();

      /* hydrograph already exists? */
      for( final IBranchHydrograph hydrograph : hydrographs )
      {
        final String id = hydrograph.getBranchId();

        if( branch.getId().equals( id ) )
          return hydrograph;
      }

      /* create a new hydrograph */
      final IBranchHydrograph hydograph = BranchHydrographUtilities.createHydrograph( m_resultModelHandler, m_workspace, branch );

      // save changes
      final IFile iFile = ResultModelHelper.getBranchHydrogrographWorkspaceFile( m_resultModelHandler.getResultFolder() );
      GmlSerializer.serializeWorkspace( iFile.getLocation().toFile(), m_workspace, "UTF-8" ); //$NON-NLS-1$

      return hydograph;
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  public IBranchHydrograph[] getHydrographs( )
  {
    final List<IBranchHydrograph> myList = new ArrayList<IBranchHydrograph>();

    for( final Object obj : this )
    {
      if( !(obj instanceof Feature) )
        continue;

      myList.add( new BranchHydrographHandler( (Feature) obj ) );
    }

    return myList.toArray( new IBranchHydrograph[] {} );
  }

}
