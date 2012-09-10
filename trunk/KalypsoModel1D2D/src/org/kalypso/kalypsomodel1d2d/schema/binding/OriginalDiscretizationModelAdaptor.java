package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.util.pool.IModelAdaptor;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.functions.GeometryCalcControl;
import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.command.FeatureListChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Adapter from original 1d2d discretization model (containing inverted edges) to version 1.0 without.
 *
 * @author kurzbach
 */
public class OriginalDiscretizationModelAdaptor implements IModelAdaptor
{
  private static final String VERSION_1_0 = "1.0"; //$NON-NLS-1$

  private IStatus m_result = Status.OK_STATUS;

  @Override
  public GMLWorkspace adapt( final GMLWorkspace workspace, final IProgressMonitor monitor )
  {
    final Object property = workspace.getRootFeature().getProperty( VersionedModel.SIM_BASE_PROP_VERSION );
    if( property != null )
    {
      // no need to adapt any other models than those without the version property
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
    final IStatusCollector log = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

    final Feature model = workspace.getRootFeature();
    final IFeatureType modelFeatureType = model.getFeatureType();

    final IRelationType complexElementsProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_COMPLEX_ELEMENTS );
    final IRelationType elementsProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );
    final IRelationType edgesProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_EDGES );
    final IRelationType continuityLinesProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_CONTINUITY_LINES );
    final IRelationType nodesProperty = (IRelationType) modelFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_NODES );

    final FeatureList complexElements = (FeatureList) model.getProperty( complexElementsProperty );
    final FeatureList elements = (FeatureList) model.getProperty( elementsProperty );
    final FeatureList edges = (FeatureList) model.getProperty( edgesProperty );
    final FeatureList nodes = (FeatureList) model.getProperty( nodesProperty );
    final FeatureList continuityLines = (FeatureList) model.getProperty( continuityLinesProperty );

    try
    {
      final int numberOfChanges = complexElements.size() + elements.size() + continuityLines.size() + nodes.size();
      final List<FeatureChange> featureChanges = new ArrayList<>( numberOfChanges );
      final int amountOfWork = numberOfChanges * 10;
      monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.1" ), amountOfWork ); //$NON-NLS-1$

      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.2" ) ); //$NON-NLS-1$
      final Map<String, Feature> allNodes = new HashMap<>( nodes.size() );
      final Map<String, Feature> allEdges = new HashMap<>( edges.size() );
      final Map<String, Feature> allElements = new HashMap<>( elements.size() );
      final Map<String, Feature> allContinuityLines = new HashMap<>( continuityLines.size() );

      // check all elements and collect edges and nodes
      for( final Object elementOrLink : elements )
      {
        final Feature element = FeatureHelper.getFeature( workspace, elementOrLink );

        // ignore two elements with the same id
        final String id = element.getId();
        if( allElements.containsKey( id ) )
          continue;

        // distinguish between PolyElement and 1dElement
        final IFeatureType elementFeatureType = element.getFeatureType();
        final QName elementQName = elementFeatureType.getQName();

        if( elementQName.equals( IPolyElement.QNAME ) )
        {
          if( checkPolyElement( element, allEdges, allNodes, featureChanges, log ) )
            allElements.put( id, element );
        }
        else if( elementQName.equals( IElement1D.QNAME ) )
        {
          if( check1dElement( element, allEdges, allNodes ) )
            allElements.put( id, element );
        }
        else
          throw new IllegalStateException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.3" ) + elementQName ); //$NON-NLS-1$
        monitor.worked( 10 );
      }

      for( final Object complexElementOrLink : complexElements )
      {
        final Feature complexElement = FeatureHelper.getFeature( workspace, complexElementOrLink );

        // ignore two complexElements with the same id
        final String id = complexElement.getId();
        if( allElements.containsKey( id ) )
          continue;

        // distinguish between complex elements
        final IFeatureType complexElementFeatureType = complexElement.getFeatureType();
        final QName complexElementQName = complexElementFeatureType.getQName();

        if( complexElementQName.equals( ICalculationUnit2D.QNAME ) )
          checkComplexElement2d( complexElement, featureChanges, log );
        monitor.worked( 10 );
      }

      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.4" ) ); //$NON-NLS-1$
      for( final Object continuityLineOrLink : continuityLines )
      {
        final Feature continuityLine = FeatureHelper.getFeature( workspace, continuityLineOrLink );

        // ignore two continuity lines with the same id
        final String id = continuityLine.getId();
        if( allContinuityLines.containsKey( id ) )
          continue;

        // distinguish between 1d and 2d
        final IFeatureType continuityLineFeatureType = continuityLine.getFeatureType();
        final QName continuityLineQName = continuityLineFeatureType.getQName();
        if( continuityLineQName.equals( IContinuityLine2D.QNAME ) )
        {
          if( checkContinuityLine( continuityLine, allNodes ) )
            allContinuityLines.put( id, continuityLine );
        }
        else if( continuityLineQName.equals( IContinuityLine1D.QNAME ) )
        {
          if( checkContinuityLine( continuityLine, allNodes ) )
            allContinuityLines.put( id, continuityLine );
        }
        else
        {
          throw new IllegalStateException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.5" ) + continuityLineQName ); //$NON-NLS-1$
        }
        monitor.worked( 10 );
      }

      for( final Feature node : allNodes.values() )
      {
        final IFeatureType nodeFeatureType = node.getFeatureType();

        final IRelationType nodeContainersProperty = (IRelationType) nodeFeatureType.getProperty( IFE1D2DNode.MEMBER_NODE_CONTAINERS );
        final FeatureList nodeContainers = (FeatureList) node.getProperty( nodeContainersProperty );

        final List<String> newContainers = new ArrayList<>();
        nextNode: for( final Feature nodeContainer : getFeatures( nodeContainers ) )
        {
          final IFeatureType nodeContainerFeatureType = nodeContainer.getFeatureType();
          final QName nodeContainerQName = nodeContainerFeatureType.getQName();
          if( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV.equals( nodeContainerQName ) )
          {
            // InvEdge
            // just ignore references to inverted edges
            continue nextNode;
          }
          else if( IFE1D2DEdge.QNAME.equals( nodeContainerQName ) )
          {
            // Edge
            if( !allEdges.containsKey( nodeContainer.getId() ) )
            {
              log.add( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.6", nodeContainer, node ) ); //$NON-NLS-1$
              continue nextNode;
            }

            final FeatureList myNodeLinks = (FeatureList) nodeContainer.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );

            final Feature[] myNodes = getFeatures( myNodeLinks );

            final int nodeCount = myNodes.length;
            if( nodeCount != 2 )
            {
              log.add( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.7", nodeContainer, nodeCount ) ) ; //$NON-NLS-1$
              continue nextNode;
            }

            final Object p0 = myNodes[0].getProperty( IFE1D2DNode.PROPERTY_POINT );
            final Object p1 = myNodes[1].getProperty( IFE1D2DNode.PROPERTY_POINT );

            for( final String otherEdgeLink : newContainers )
            {
              final Feature otherEdge = FeatureHelper.getFeature( workspace, otherEdgeLink );
              if( !otherEdge.getFeatureType().getQName().equals( IFE1D2DEdge.QNAME ) )
                continue;

              final FeatureList otherNodeLinks = (FeatureList) otherEdge.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );

              final Feature[] otherNodes = getFeatures( otherNodeLinks );
              final Object op0 = otherNodes[0].getProperty( IFE1D2DNode.PROPERTY_POINT );
              final Object op1 = otherNodes[1].getProperty( IFE1D2DNode.PROPERTY_POINT );

              if( (p0.equals( op0 ) && p1.equals( op1 )) || (p0.equals( op1 ) && p1.equals( op0 )) )
              {
                log.add( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.8", otherEdge, nodeContainer ) ) ; //$NON-NLS-1$
                continue nextNode;
              }
            }
          }
          else if( IContinuityLine1D.QNAME.equals( nodeContainerQName ) || IContinuityLine2D.QNAME.equals( nodeContainerQName ) )
          {
            // ContinuityLine 1d/2d
          }
          else
          {
            throw new IllegalStateException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.9" ) + nodeContainerQName ); //$NON-NLS-1$
          }

          newContainers.add( nodeContainer.getId() );
        }

        featureChanges.add( new FeatureListChange( node, nodeContainersProperty, newContainers ) );
        monitor.worked( 10 );
      }

      // set newly calculated elements, edges, nodes and continuity lines
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.10" ) ); //$NON-NLS-1$
      featureChanges.add( 0, new FeatureListChange( model, nodesProperty, new ArrayList<>( allNodes.values() ) ) );
      featureChanges.add( 0, new FeatureListChange( model, elementsProperty, new ArrayList<>( allElements.values() ) ) );
      featureChanges.add( 0, new FeatureListChange( model, edgesProperty, new ArrayList<>( allEdges.values() ) ) );
      featureChanges.add( 0, new FeatureListChange( model, continuityLinesProperty, new ArrayList<>( allContinuityLines.values() ) ) );

      // set version
      final IPropertyType versionProperty = modelFeatureType.getProperty( VersionedModel.SIM_BASE_PROP_VERSION );
      featureChanges.add( new FeatureChange( model, versionProperty, VERSION_1_0 ) );

      // create command
      final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand( workspace, featureChanges.toArray( new FeatureChange[featureChanges.size()] ) );
      changeFeaturesCommand.process();

      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.11" ) ); //$NON-NLS-1$

      GeometryCalcControl.setDoCalcElement( false );
      GeometryCalcControl.setDoCalcEdge( false );
    }
    catch( final Exception e )
    {

      log.add( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.loader.GmlLoader.12" ) ); //$NON-NLS-1$
      log.add( StatusUtilities.statusFromThrowable( e ) );

      // update the gml version even in failure case, to avoid the dead-lock recursion
      final List<FeatureChange> featureChanges = new ArrayList<>( 1 );
      final IPropertyType versionProperty = modelFeatureType.getProperty( VersionedModel.SIM_BASE_PROP_VERSION );
      featureChanges.add( new FeatureChange( model, versionProperty, VERSION_1_0 ) );
      final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand( workspace, featureChanges.toArray( new FeatureChange[featureChanges.size()] ) );
      try
      {
        changeFeaturesCommand.process();
      }
      catch( final Exception e1 )
      {
        // ... well, FIXME?
      }

    }
    finally
    {
      GeometryCalcControl.setDoCalcElement( true );
      GeometryCalcControl.setDoCalcEdge( true );
      // elements.invalidate();
      monitor.done();
    }

    if( log.size() > 0 )
      return log.asMultiStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.12" ) ); //$NON-NLS-1$
    else
      return new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.13" ) ); //$NON-NLS-1$
  }

  private void checkComplexElement2d( final Feature complexElement, final List<FeatureChange> collectChanges, final IStatusCollector log )
  {
    final IFeatureType complexElementFeatureType = complexElement.getFeatureType();
    final IRelationType elementsProperty = (IRelationType) complexElementFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );
    final FeatureList elementsInComplexElement = (FeatureList) complexElement.getProperty( elementsProperty );
    final int numberOfElements = elementsInComplexElement.size();

    final List<String> newElements = new ArrayList<>( numberOfElements );
    for( final Object elementOrId : elementsInComplexElement )
    {
      final Feature elementFeature = FeatureHelper.getFeature( complexElement.getWorkspace(), elementOrId );
      if( elementFeature == null )
      {
        // feature does not exist
        log.add( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.14", elementOrId, complexElement ) ); //$NON-NLS-1$
        continue;
      }

      final String id = (String) elementOrId;
      newElements.add( id );
    }

    collectChanges.add( new FeatureListChange( complexElement, elementsProperty, newElements ) );
  }

  private Feature[] getFeatures( final FeatureList featureList )
  {
    final GMLWorkspace workspace = featureList.getOwner().getWorkspace();

    final Feature[] result = new Feature[featureList.size()];
    int counter = 0;
    for( final Object link : featureList )
      result[counter++] = FeatureHelper.getFeature( workspace, link );
    return result;
  }

  private boolean checkContinuityLine( final Feature element, final Map<String, Feature> collectNodes )
  {
    final FeatureList nodeList = (FeatureList) element.getProperty( IFELine.PROP_NODES );
    for( final Feature node : getFeatures( nodeList ) )
    {
      final String id = node.getId();
      if( !collectNodes.containsKey( id ) )
      {
        throw new IllegalStateException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.15" ) + node ); //$NON-NLS-1$
      }
    }
    return true;
  }

  private boolean check1dElement( final Feature element, final Map<String, Feature> collectEdges, final Map<String, Feature> collectNodes )
  {
    try
    {
      final IFeatureType elementFeatureType = element.getFeatureType();
      final IRelationType edgeProperty = (IRelationType) elementFeatureType.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
      final String edgeInElementLink = (String) element.getProperty( edgeProperty );
      final Feature edge = FeatureHelper.getFeature( element.getWorkspace(), edgeInElementLink );
      final FeatureList nodeList = (FeatureList) edge.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );
      checkNodes( getFeatures( nodeList ), collectNodes );

      final String id = edge.getId();
      if( !collectEdges.containsKey( id ) )
        collectEdges.put( id, edge );
    }
    catch( final Exception e )
    {
      return false;
    }
    return true;
  }

  private void checkNodes( final Feature[] nodes, final Map<String, Feature> collectNodes )
  {
    for( final Feature node : nodes )
    {
      final String id = node.getId();
      if( !collectNodes.containsKey( id ) )
        collectNodes.put( id, node );
    }
  }

  private boolean checkPolyElement( final Feature element, final Map<String, Feature> collectEdges, final Map<String, Feature> collectNodes, final Collection<FeatureChange> collectChanges, final IStatusCollector log )
  {
    final IFeatureType elementFeatureType = element.getFeatureType();
    final IRelationType edgesProperty = (IRelationType) elementFeatureType.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
    final FeatureList edgesInElement = (FeatureList) element.getProperty( edgesProperty );

    final int numberOfEdges = edgesInElement.size();
    if( numberOfEdges < 3 )
    {
      log.add( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.16", element ) ); //$NON-NLS-1$
      return false;
    }
    if( numberOfEdges > 4 )
    {
      log.add( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.17", element ) ); //$NON-NLS-1$
      return false;
    }

    // remove inverted edges from elements and add corresponding normal edges instead
    final List<String> newEdges = new ArrayList<>( numberOfEdges );
    for( Feature edgeOrInvEdge : getFeatures( edgesInElement ) )
    {
      final IFeatureType edgeFeatureType = edgeOrInvEdge.getFeatureType();
      final QName edgeQName = edgeFeatureType.getQName();

      if( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV.equals( edgeQName ) )
        // InvEdge
        edgeOrInvEdge = checkInvEdge( edgeOrInvEdge, collectChanges );
      else if( IFE1D2DEdge.QNAME.equals( edgeQName ) )
        // remove edgeInv property
        edgeOrInvEdge.setProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGEINV, null );
      else
        throw new IllegalStateException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.18" ) + edgeQName ); //$NON-NLS-1$

      final String id = edgeOrInvEdge.getId();
      if( !newEdges.contains( id ) )
        newEdges.add( id );
      else
        log.add( IStatus.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.OriginalDiscretizationModelAdaptor.19", id, element ) ); //$NON-NLS-1$

      if( !collectEdges.containsKey( id ) )
        collectEdges.put( id, edgeOrInvEdge );

      final FeatureList nodeList = (FeatureList) edgeOrInvEdge.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );
      checkNodes( getFeatures( nodeList ), collectNodes );
    }

    collectChanges.add( new FeatureListChange( element, edgesProperty, newEdges ) );
    return true;
  }

  private Feature checkInvEdge( final Feature edge, final Collection<FeatureChange> collectChanges )
  {
    final IFeatureType edgeFeatureType = edge.getFeatureType();
    final Object invEdgeLink = edge.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV );
    final Feature invEdge = FeatureHelper.getFeature( edge.getWorkspace(), invEdgeLink );

    // add all references to elements from inverted edges to corresponding normal edges
    final IRelationType elementsProperty = (IRelationType) edgeFeatureType.getProperty( IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );
    final List<String> elements = (FeatureList) edge.getProperty( elementsProperty );
    final List<String> elementsInInvEdge = (FeatureList) invEdge.getProperty( elementsProperty );

    final List<String> newElements = new ArrayList<>();
    newElements.addAll( elements );
    newElements.addAll( elementsInInvEdge );

    collectChanges.add( new FeatureListChange( invEdge, elementsProperty, newElements ) );
    return invEdge;
  }
}