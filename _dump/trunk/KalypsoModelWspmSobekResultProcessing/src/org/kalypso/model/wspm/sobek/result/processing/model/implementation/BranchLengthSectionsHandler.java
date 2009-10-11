package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.result.processing.SobekResultModelHandler;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchLengthSection;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchLengthSectionModel;
import org.kalypso.model.wspm.sobek.result.processing.utils.BranchLengthSectionUtilities;
import org.kalypso.model.wspm.sobek.result.processing.utils.ResultModelHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

public class BranchLengthSectionsHandler extends AbstractListWrapper implements IBranchLengthSectionModel
{
  private final CommandableWorkspace m_workspace;

  private final SobekResultModelHandler m_resultModelHandler;

  public BranchLengthSectionsHandler( final SobekResultModelHandler resultModelHandler, final CommandableWorkspace workspace, final FeatureList list )
  {
    super( list );
    m_resultModelHandler = resultModelHandler;

    m_workspace = workspace;
  }

  public IBranchLengthSection getHydrograph( final IBranch branch ) throws CoreException
  {
    try
    {
      final IBranchLengthSection[] sections = getLengthSections();

      /* hydrograph already exists? */
      for( final IBranchLengthSection section : sections )
      {
        final String id = section.getBranchId();

        if( branch.getId().equals( id ) )
          return section;
      }

      /* create a new hydrograph */
      final IBranchLengthSection hydograph = BranchLengthSectionUtilities.createLengthSection( m_resultModelHandler, m_workspace, branch );

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

  public IBranchLengthSection[] getLengthSections( )
  {
    final List<IBranchLengthSection> myList = new ArrayList<IBranchLengthSection>();

    for( final Object obj : this )
    {
      if( !(obj instanceof Feature) )
        continue;

      myList.add( new BranchLengthSectionHandler( (Feature) obj ) );
    }

    return myList.toArray( new IBranchLengthSection[] {} );
  }

  @Override
  public Feature addNew( final QName newChildType )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public Feature addNew( final QName newChildType, final String newFeatureId )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T addNew( final QName newChildType, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T addNew( final QName newChildType, final String newFeatureId, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> boolean addRef( final T toAdd ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public Feature insertNew( final int index, final QName newChildType )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public Feature insertNew( final int index, final QName newChildType, final String newFeatureId )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T insertNew( final int index, final QName newChildType, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T insertNew( final int index, final QName newChildType, final String newFeatureId, final Class<T> classToAdapt )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> T insertNew( final int index, final QName newChildType, final String newFeatureId, final Class<T> classToAdapt, final Object[] properties )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public <T extends Feature> boolean insertRef( final int index, final T toAdd ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public List<Feature> queryIntersectResolve( final GM_Envelope env, final List<Feature> result )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<Feature> queryIntersectResolve( final GM_Point point, final List<Feature> result )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
