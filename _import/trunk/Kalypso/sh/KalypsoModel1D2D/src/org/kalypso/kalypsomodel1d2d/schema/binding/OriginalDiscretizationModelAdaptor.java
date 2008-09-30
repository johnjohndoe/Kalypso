package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.util.pool.IModelAdaptor;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
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
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.command.FeatureListChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Adapter from original 1d2d discretization model (containing inverted edges) to version 1.0 without.
 * 
 * @author kurzbach
 */
public class OriginalDiscretizationModelAdaptor implements IModelAdaptor
{
  private static final String VERSION_1_0 = "1.0";

  private IStatus m_result = Status.OK_STATUS;

  public CommandableWorkspace adapt( final CommandableWorkspace workspace )
  {
    final Object property = workspace.getRootFeature().getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_VERSION );
    if( property != null )
    {
      // no need to adapt any other models than those without the version property
      return workspace;
    }

    final String name = "Diskretisierungsmodell adaptieren";
    final Job job = new Job( name )
    {

      @Override
      protected IStatus run( IProgressMonitor monitor )
      {
        if( monitor == null )
        {
          monitor = new NullProgressMonitor();
        }
        final List<IStatus> statusList = new ArrayList<IStatus>();

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
          final CompositeCommand cc = new CompositeCommand( name );

          final int numberOfChanges = complexElements.size() + elements.size() + continuityLines.size() + nodes.size();
          final List<FeatureChange> featureChanges = new ArrayList<FeatureChange>( numberOfChanges );
          final int amountOfWork = numberOfChanges * 10;
          monitor.beginTask( "Diskretisierungsmodell adaptieren", amountOfWork );

          monitor.subTask( "bearbeite Elemente" );
          final Map<String, Feature> allNodes = new HashMap<String, Feature>( nodes.size() );
          final Map<String, Feature> allEdges = new HashMap<String, Feature>( edges.size() );
          final Map<String, Feature> allElements = new HashMap<String, Feature>( elements.size() );
          final Map<String, Feature> allContinuityLines = new HashMap<String, Feature>( continuityLines.size() );

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
              if( checkPolyElement( element, allEdges, allNodes, featureChanges, statusList ) )
                allElements.put( id, element );
            }
            else if( elementQName.equals( IElement1D.QNAME ) )
            {
              if( check1dElement( element, allEdges, allNodes, featureChanges ) )
                allElements.put( id, element );
            }
            else
              throw new IllegalStateException( "element type not handled: " + elementQName );
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
              checkComplexElement2d( complexElement, featureChanges, statusList );
            monitor.worked( 10 );
          }

          monitor.subTask( "bearbeite Kontinuitätslinien" );
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
              if( checkContinuityLine( continuityLine, allNodes, featureChanges ) )
                allContinuityLines.put( id, continuityLine );
            }
            else if( continuityLineQName.equals( IContinuityLine1D.QNAME ) )
            {
              if( checkContinuityLine( continuityLine, allNodes, featureChanges ) )
                allContinuityLines.put( id, continuityLine );
            }
            else
            {
              throw new IllegalStateException( "continuity line type not handled: " + continuityLineQName );
            }
            monitor.worked( 10 );
          }

          for( final Feature node : allNodes.values() )
          {
            final IFeatureType nodeFeatureType = node.getFeatureType();

            final IRelationType nodeContainersProperty = (IRelationType) nodeFeatureType.getProperty( IFE1D2DNode.WB1D2D_PROP_NODE_CONTAINERS );
            final FeatureList nodeContainers = (FeatureList) node.getProperty( nodeContainersProperty );
            final List<String> newContainers = new ArrayList<String>();
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
                  statusList.add( StatusUtilities.createWarningStatus( String.format( "ignoring reference to edge %s in node %s", nodeContainer, node ) ) );
                  continue nextNode;
                }

                final FeatureList myNodeLinks = (FeatureList) nodeContainer.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );

                final Feature[] myNodes = getFeatures( myNodeLinks );

                final int nodeCount = myNodes.length;
                if( nodeCount != 2 )
                {
                  statusList.add( StatusUtilities.createWarningStatus( String.format( "edge %s has %d nodes", nodeContainer, nodeCount ) ) );
                  continue nextNode;
                }

                final Object p0 = myNodes[0].getProperty( IFE1D2DNode.WB1D2D_PROP_POINT );
                final Object p1 = myNodes[1].getProperty( IFE1D2DNode.WB1D2D_PROP_POINT );

                for( final String otherEdgeLink : newContainers )
                {
                  final Feature otherEdge = FeatureHelper.getFeature( workspace, otherEdgeLink );
                  if( !otherEdge.getFeatureType().getQName().equals( IFE1D2DEdge.QNAME ) )
                    continue;

                  final FeatureList otherNodeLinks = (FeatureList) otherEdge.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );

                  final Feature[] otherNodes = getFeatures( otherNodeLinks );
                  final Object op0 = otherNodes[0].getProperty( IFE1D2DNode.WB1D2D_PROP_POINT );
                  final Object op1 = otherNodes[1].getProperty( IFE1D2DNode.WB1D2D_PROP_POINT );

                  if( (p0.equals( op0 ) && p1.equals( op1 )) || (p0.equals( op1 ) && p1.equals( op0 )) )
                  {
                    statusList.add( StatusUtilities.createWarningStatus( String.format( "equal edges: %s and %s", otherEdge, nodeContainer ) ) );
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
                throw new IllegalStateException( "node container type not handled: " + nodeContainerQName );
              }

              newContainers.add( nodeContainer.getId() );
            }

            featureChanges.add( new FeatureListChange( node, nodeContainersProperty, newContainers ) );
            monitor.worked( 10 );
          }

          // set newly calculated elements, edges, nodes and continuity lines
          monitor.subTask( "berechne Änderungen" );
          featureChanges.add( 0, new FeatureListChange( model, nodesProperty, new ArrayList<Feature>( allNodes.values() ) ) );
          featureChanges.add( 0, new FeatureListChange( model, elementsProperty, new ArrayList<Feature>( allElements.values() ) ) );
          featureChanges.add( 0, new FeatureListChange( model, edgesProperty, new ArrayList<Feature>( allEdges.values() ) ) );
          featureChanges.add( 0, new FeatureListChange( model, continuityLinesProperty, new ArrayList<Feature>( allContinuityLines.values() ) ) );

          // set version
          final IPropertyType versionProperty = modelFeatureType.getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_VERSION );
          featureChanges.add( new FeatureChange( model, versionProperty, VERSION_1_0 ) );

          // create command
          final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand( workspace, featureChanges.toArray( new FeatureChange[featureChanges.size()] ) );
          cc.addCommand( changeFeaturesCommand );

          monitor.subTask( "übernehme Änderungen" );

          GeometryCalcControl.setDoCalcElement( false );
          GeometryCalcControl.setDoCalcEdge( false );
          workspace.postCommand( cc );
        }
        catch( final Exception e )
        {
          statusList.add( StatusUtilities.statusFromThrowable( e ) );
        }
        finally
        {
          GeometryCalcControl.setDoCalcElement( true );
          GeometryCalcControl.setDoCalcEdge( true );
          // elements.invalidate();
          monitor.done();
        }
        final IStatus resultStatus;
        if( statusList.size() > 0 )
          resultStatus = StatusUtilities.createStatus( statusList, "Probleme beim Adaptieren des Modells." );
        else
          resultStatus = StatusUtilities.createInfoStatus( "Diskretisierungsnetz wurde erfolgreich auf Version 1.0 adaptiert." );
        return resultStatus;
      }

      private void checkComplexElement2d( final Feature complexElement, final List<FeatureChange> collectChanges, final List<IStatus> statusList )
      {
        final IFeatureType complexElementFeatureType = complexElement.getFeatureType();
        final IRelationType elementsProperty = (IRelationType) complexElementFeatureType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );
        final FeatureList elementsInComplexElement = (FeatureList) complexElement.getProperty( elementsProperty );
        final int numberOfElements = elementsInComplexElement.size();

        final List<String> newElements = new ArrayList<String>( numberOfElements );
        for( final Object elementOrId : elementsInComplexElement )
        {
          final Feature elementFeature = FeatureHelper.getFeature( workspace, elementOrId );
          if( elementFeature == null )
          {
            // feature does not exist
            statusList.add( StatusUtilities.createWarningStatus( String.format( "Ignoriere Referenz auf nicht existierendes Element mit der Id %s in der Berechnungseinheit %s", elementOrId, complexElement ) ) );
            continue;
          }

          final String id = (String) elementOrId;
          // final QName elementQName = element.getFeatureType().getQName();
          // if( elementQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT ) )
          // {
          //
          // if( !allElements.containsKey( id ) )
          // {
          // System.out.println( "ignoring reference to non-existing PolyElement " + id + " in complex element " +
          // complexElement );
          // continue;
          // }
          // }
          newElements.add( id );
        }

        collectChanges.add( new FeatureListChange( complexElement, elementsProperty, newElements ) );
      }

      private Feature[] getFeatures( final FeatureList featureList )
      {
        final Feature[] result = new Feature[featureList.size()];
        int counter = 0;
        for( final Object link : featureList )
          result[counter++] = FeatureHelper.getFeature( workspace, link );
        return result;
      }

      private boolean checkContinuityLine( final Feature element, final Map<String, Feature> collectNodes, final Collection<FeatureChange> collectChanges )
      {
        final FeatureList nodeList = (FeatureList) element.getProperty( IFELine.PROP_NODES );
        for( final Feature node : getFeatures( nodeList ) )
        {
          final String id = node.getId();
          if( !collectNodes.containsKey( id ) )
          {
            throw new IllegalStateException( "continuity line references node not in list: " + node );
          }
        }
        return true;
      }

      private boolean check1dElement( final Feature element, final Map<String, Feature> collectEdges, final Map<String, Feature> collectNodes, final Collection<FeatureChange> collectChanges )
      {
        final IFeatureType elementFeatureType = element.getFeatureType();
        final IRelationType edgeProperty = (IRelationType) elementFeatureType.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
        final String edgeInElementLink = (String) element.getProperty( edgeProperty );
        final Feature edge = FeatureHelper.getFeature( workspace, edgeInElementLink );
        final FeatureList nodeList = (FeatureList) edge.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );
        checkNodes( getFeatures( nodeList ), collectNodes, collectChanges );

        final String id = edge.getId();
        if( !collectEdges.containsKey( id ) )
          collectEdges.put( id, edge );
        return true;
      }

      private void checkNodes( final Feature[] nodes, final Map<String, Feature> collectNodes, final Collection<FeatureChange> collectChanges )
      {
        for( final Feature node : nodes )
        {
          final String id = node.getId();
          if( !collectNodes.containsKey( id ) )
            collectNodes.put( id, node );
        }
      }

      private boolean checkPolyElement( final Feature element, final Map<String, Feature> collectEdges, final Map<String, Feature> collectNodes, final Collection<FeatureChange> collectChanges, final List<IStatus> statusList )
      {
        final IFeatureType elementFeatureType = element.getFeatureType();
        final IRelationType edgesProperty = (IRelationType) elementFeatureType.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
        final FeatureList edgesInElement = (FeatureList) element.getProperty( edgesProperty );

        final int numberOfEdges = edgesInElement.size();
        if( numberOfEdges < 3 )
        {
          statusList.add( StatusUtilities.createWarningStatus( String.format( "Ignoriere PolyElement %s mit weniger als 3 Kanten.", element ) ) );
          return false;
        }
        if( numberOfEdges > 4 )
        {
          statusList.add( StatusUtilities.createWarningStatus( String.format( "Ignoriere PolyElement %s mit mehr als 4 Kanten.", element ) ) );
          return false;
        }

        // remove inverted edges from elements and add corresponding normal edges instead
        final List<String> newEdges = new ArrayList<String>( numberOfEdges );
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
            throw new IllegalStateException( "edge type not handled: " + edgeQName );

          final String id = edgeOrInvEdge.getId();
          if( !newEdges.contains( id ) )
            newEdges.add( id );
          else
            statusList.add( StatusUtilities.createWarningStatus( String.format( "Ignoriere doppelte Referenz auf Kante %s in PolyElement %s.", id, element ) ) );
          System.out.println();
          if( !collectEdges.containsKey( id ) )
            collectEdges.put( id, edgeOrInvEdge );

          final FeatureList nodeList = (FeatureList) edgeOrInvEdge.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );
          checkNodes( getFeatures( nodeList ), collectNodes, collectChanges );
        }

        collectChanges.add( new FeatureListChange( element, edgesProperty, newEdges ) );
        return true;
      }

      @SuppressWarnings("unchecked")
      private Feature checkInvEdge( final Feature edge, final Collection<FeatureChange> collectChanges )
      {
        final IFeatureType edgeFeatureType = edge.getFeatureType();
        final Object invEdgeLink = edge.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV );
        final Feature invEdge = FeatureHelper.getFeature( workspace, invEdgeLink );

        // add all references to elements from inverted edges to corresponding normal edges
        final IRelationType elementsProperty = (IRelationType) edgeFeatureType.getProperty( IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );
        final List<String> elements = (FeatureList) edge.getProperty( elementsProperty );
        final List<String> elementsInInvEdge = (FeatureList) invEdge.getProperty( elementsProperty );

        final List<String> newElements = new ArrayList<String>();
        newElements.addAll( elements );
        newElements.addAll( elementsInInvEdge );

        collectChanges.add( new FeatureListChange( invEdge, elementsProperty, newElements ) );
        return invEdge;
      }
    };

    job.schedule();
    try
    {
      job.join();
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }

    m_result = job.getResult();
    return workspace;
  }

  public IStatus getResult( )
  {
    return m_result;
  }
}
