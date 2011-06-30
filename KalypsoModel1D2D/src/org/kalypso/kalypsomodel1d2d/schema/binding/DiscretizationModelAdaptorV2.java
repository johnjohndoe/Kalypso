package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.util.pool.IModelAdaptor;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Adapter from version 1.0 1d2d discretization model to version 2.0
 * 
 * @author kurzbach
 */
public class DiscretizationModelAdaptorV2 implements IModelAdaptor
{
  private static final String VERSION_1_0 = "1.0"; //$NON-NLS-1$

  private static final String VERSION_2_0 = "2.0"; //$NON-NLS-1$

  private IStatus m_result = Status.OK_STATUS;

  @Override
  public GMLWorkspace adapt( final GMLWorkspace workspace, final IProgressMonitor monitor )
  {
    final Object property = workspace.getRootFeature().getProperty( VersionedModel.SIM_BASE_PROP_VERSION );
    if( !VERSION_1_0.equals( property ) )
    {
      // only adapt version 1.0
      return workspace;
    }

    m_result = execute( workspace, monitor );

    return workspace;
  }

  @Override
  public IStatus getResult( )
  {
    return m_result;
  }

  protected IStatus execute( final GMLWorkspace workspace, final IProgressMonitor monitor )
  {
    final List<IStatus> statusList = new ArrayList<IStatus>();

    final Feature model = workspace.getRootFeature();
    final IFeatureType modelFeatureType = model.getFeatureType();

    final IRelationType complexElementsProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_COMPLEX_ELEMENTS );
    final IRelationType elementsProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );
    final IRelationType edgesProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_EDGES );
    final IRelationType continuityLinesProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_CONTINUITY_LINES );
    final IRelationType nodesProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_NODES );

    final FeatureList complexElements = (FeatureList) model.getProperty( complexElementsProperty );
    final FeatureList continuityLines = (FeatureList) model.getProperty( continuityLinesProperty );

    final FeatureList elements = (FeatureList) model.getProperty( elementsProperty );
    final FeatureList edges = (FeatureList) model.getProperty( edgesProperty );
    final FeatureList nodes = (FeatureList) model.getProperty( nodesProperty );

    try
    {
      final IRelationType meshProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_MESH );
      final Feature mesh = workspace.createFeature( model, meshProperty, meshProperty.getTargetFeatureType() );
      final GM_Surface<GM_Polygon> surface = GeometryFactory.createGM_PolyhedralSurface( new GM_Polygon[0], KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

      final IFEDiscretisationModel1d2d discModel = (IFEDiscretisationModel1d2d) model.getAdapter( IFEDiscretisationModel1d2d.class );
      final IFeatureWrapperCollection<IFE1D2DElement> elements2 = discModel.getElements();
      for( IFE1D2DElement element : elements2 )
      {
        if( element instanceof PolyElement )
        {
          final GM_Surface<GM_SurfacePatch> geometry = ((PolyElement) element).getGeometry();
          final GM_SurfacePatch sp = geometry.get( 0 );
          surface.add( (GM_Polygon) sp );
        }
      }
      mesh.setProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_MESH_SURFACE, surface );

      final int amountOfWork = 100;
      final List<FeatureChange> featureChanges = new ArrayList<FeatureChange>( 1 );
      monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.1" ), amountOfWork ); //$NON-NLS-1$

      // clear old values
      elements.clear();
      edges.clear();
      nodes.clear();

      // create command
      featureChanges.add( new FeatureChange( model, meshProperty, mesh ) );
      final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand( workspace, featureChanges.toArray( new FeatureChange[featureChanges.size()] ) );
      changeFeaturesCommand.process();
    }
    catch( final Exception e )
    {
      statusList.add( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      monitor.done();
    }

    final IStatus resultStatus;
    if( statusList.size() > 0 )
      resultStatus = StatusUtilities.createStatus( statusList, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.12" ) ); //$NON-NLS-1$
    else
      resultStatus = StatusUtilities.createInfoStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.13" ) ); //$NON-NLS-1$
    return resultStatus;
  }
}
