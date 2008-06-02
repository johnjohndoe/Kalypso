package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrograph;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrographModel;
import org.kalypso.model.wspm.sobek.result.processing.utils.BranchHydrographUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public class BranchHydraphModelHandler extends AbstractListWrapper implements IBranchHydrographModel
{
  private final CommandableWorkspace m_workspace;

  public BranchHydraphModelHandler( final CommandableWorkspace workspace, final FeatureList list )
  {
    super( list );

    m_workspace = workspace;
  }

  public IBranchHydrograph getHydrograph( final IBranch branch ) throws CoreException
  {
    IBranchHydrograph[] hydrographs = getHydrographs();

    /* hydrograph already exists? */
    for( IBranchHydrograph hydrograph : hydrographs )
    {
      String id = hydrograph.getBranchId();

      if( branch.getId().equals( id ) )
        return hydrograph;
    }

    /* create a new hydrograph */
    IBranchHydrograph hydograph = BranchHydrographUtilities.createHydrograph( m_workspace, branch );

    return null;
  }

  public IBranchHydrograph[] getHydrographs( )
  {
    List<IBranchHydrograph> myList = new ArrayList<IBranchHydrograph>();

    for( Object obj : this )
    {
      if( !(obj instanceof Feature) )
        continue;

      myList.add( new BranchHydrographHandler( (Feature) obj ) );
    }

    return myList.toArray( new IBranchHydrograph[] {} );
  }

}
