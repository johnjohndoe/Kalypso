/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFENetItem;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteElement1DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Thomas Jung
 */
public class DeleteFeElementsHelper
{
  public static IStatus deleteSelectedFeElements( final IMapPanel mapPanel )
  {
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final EasyFeatureWrapper[] selected = selectionManager.getAllFeatures();
    if( selected.length == 0 )
    {
      final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.0" ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message );
    }

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.1" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.2" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return Status.OK_STATUS;

    selectionManager.clear();

    try
    {
      // to be allowed to delete the 2D element, no continuity line cannot be positioned on that element
      // to be allowed to delete the 1D element, that element cannot be the last one that touches some continuity line
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
      if( discretisationModel == null )
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.3" ) ); //$NON-NLS-1$

      final IStatus contiStatus = checkContinuityLines( discretisationModel, selected );
      if( contiStatus != null )
        return contiStatus;
      // make a list of all the nodes contained by all the continuity lines

      final IKalypsoFeatureTheme onedTheme = UtilMap.findEditableTheme( mapPanel, IFE1D2DElement.QNAME );
      final IKalypsoFeatureTheme lFlowTheme = UtilMap.findEditableTheme( mapPanel, IFlowRelationship.QNAME );

      /* Find all elements that should be deleted */
      final DeleteElement1DCmd deleteCmd1dElement = new DeleteElement1DCmd( discretisationModel );
      final DeletePolyElementCmd deleteCmdPolyElement = new DeletePolyElementCmd( discretisationModel );

      final FeatureList flowRelationsList = lFlowTheme.getFeatureList();
      final IFlowRelationshipModel lFlowRelCollection = (IFlowRelationshipModel)flowRelationsList.getOwner();

      final List<IFE1D2DElement> element1DtoRemove = new ArrayList<>();
      for( final EasyFeatureWrapper easyFeatureWrapper : selected )
      {
        final Feature feature = easyFeatureWrapper.getFeature();
        if( feature instanceof IPolyElement )
          deleteCmdPolyElement.addElementToRemove( (IPolyElement)feature );
        else if( feature instanceof IElement1D )
        {
          deleteCmd1dElement.addElementToRemove( (IElement1D)feature );
          element1DtoRemove.add( (IFE1D2DElement)feature );
        }
      }

      /* Delete all parameters that should be deleted */
      // FIXME: what about 2d parameters?
      deleteParameters( mapPanel, selected, lFlowTheme, lFlowRelCollection, element1DtoRemove, discretisationModel );

      /* Execute the delete commands */
      final CommandableWorkspace workspace = onedTheme.getWorkspace();
      workspace.postCommand( deleteCmdPolyElement );
      workspace.postCommand( deleteCmd1dElement );

      // FIXME: should have been sent by commands!
      final Set<Feature> changedFeatureList = new HashSet<>();
      changedFeatureList.addAll( Arrays.asList( deleteCmdPolyElement.getChangedFeatures() ) );
      changedFeatureList.addAll( Arrays.asList( deleteCmd1dElement.getChangedFeatures() ) );

      final Feature[] deletedFeatures = changedFeatureList.toArray( new Feature[changedFeatureList.size()] );
      final GMLWorkspace discWorkspace = discretisationModel.getWorkspace();
      final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( discWorkspace, discretisationModel, deletedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
      discWorkspace.fireModellEvent( event );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.9" ) ); //$NON-NLS-1$
    }
    return Status.OK_STATUS;
  }

  protected static void deleteParameters( final IMapPanel mapPanel, final EasyFeatureWrapper[] selected, final IKalypsoFeatureTheme lFlowTheme, final IFlowRelationshipModel lFlowRelCollection, final List<IFE1D2DElement> element1DtoRemove, final IFEDiscretisationModel1d2d discModel ) throws Exception
  {
    final List<IFlowRelationship> parametersToDelete = new ArrayList<>();

    for( final EasyFeatureWrapper easyFeatureWrapper : selected )
    {
      final Feature feature = easyFeatureWrapper.getFeature();
      if( feature instanceof IPolyElement )
      {
        final IFE1D2DElement element = (IFE1D2DElement)feature.getAdapter( IFE1D2DElement.class );
        final IBuildingFlowRelation2D toDelete = deleteParameter2D( element, lFlowRelCollection );
        parametersToDelete.addAll( Arrays.asList( toDelete ) );
      }
      else if( feature instanceof IElement1D )
      {
        final IFE1D2DElement element = (IFE1D2DElement)feature.getAdapter( IFE1D2DElement.class );
        final IFlowRelationship[] toDelete = deleteParameter1D( element, lFlowRelCollection, element1DtoRemove, discModel );
        parametersToDelete.addAll( Arrays.asList( toDelete ) );
      }
    }

    /* Really delete the parameters */
    for( final IFlowRelationship buildingElement : parametersToDelete )
    {
      if( buildingElement != null )
      {
        final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
        selectionManager.clear();

        final CompositeCommand compositeCommand = new CompositeCommand( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.14" ) ); //$NON-NLS-1$
        {
          selectionManager.changeSelection( new Feature[] { buildingElement }, new EasyFeatureWrapper[] {} );

          final DeleteFeatureCommand command = new DeleteFeatureCommand( buildingElement );
          compositeCommand.addCommand( command );
        }
        lFlowTheme.getWorkspace().postCommand( compositeCommand );

        // final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent(
        // lFlowTheme.getWorkspace(), buildingElement.getOwner(), buildingElement,
        // FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
        // lFlowTheme.getWorkspace().fireModellEvent( event );
      }
    }
  }

  private static IStatus checkContinuityLines( final IFEDiscretisationModel1d2d discretisationModel, final EasyFeatureWrapper[] selected )
  {
    final List<IFE1D2DNode> clNodes = new ArrayList<>();
    final IFELine[] continuityLines = discretisationModel.getContinuityLines();
    for( final IFELine line : continuityLines )
      clNodes.addAll( Arrays.asList( line.getNodes() ) ); // usually lines are not overlapped so there is no need to check if some of
    // the nodes are already in the list

    // 2D: check if any of the selected elements have nodes that belongs to any continuity line; if so, deleting is
    // not allowed
    // 1D: check if any of the selected elements have nodes that belongs to any continuity line; if that element is
    // the last one on the line, deleting is not allowed

    // TODO:
    // there must be a check if a node of a 1d element is also part to other 1d elements...
    for( final EasyFeatureWrapper easyFeatureWrapper : selected )
    {
      if( easyFeatureWrapper == null )
        throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.4" ) ); //$NON-NLS-1$
      final IFE1D2DElement element = (IFE1D2DElement)easyFeatureWrapper.getFeature().getAdapter( IFE1D2DElement.class );
      final IFE1D2DNode[] nodes = element.getNodes();
      for( final IFE1D2DNode node : nodes )
        if( clNodes.contains( node ) )
        {
          if( element instanceof IPolyElement )
          {
            SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.5" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            return Status.OK_STATUS;
          }
          if( element instanceof IElement1D )
          {
            final IFE1D2DEdge[] containers = node.getLinkedEdges();
            int numberOfEdgeContainers = containers.length;
            if( numberOfEdgeContainers < 2 )
            {
              SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.7" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.8" ) ); //$NON-NLS-1$ //$NON-NLS-2$
              return Status.OK_STATUS;
            }
          }
        }
    }

    return null;
  }

  private static IBuildingFlowRelation2D deleteParameter2D( final IFE1D2DElement element, final IFlowRelationshipModel lFlowRelCollection ) throws Exception
  {
    if( element instanceof IPolyElement )
      return FlowRelationUtilitites.findBuildingElement2D( (IPolyElement)element, lFlowRelCollection );
    else
      return null;
  }

  private static IFlowRelationship[] deleteParameter1D( final IFE1D2DElement element, final IFlowRelationshipModel lFlowRelCollection, final List<IFE1D2DElement> element1DtoRemove, final IFEDiscretisationModel1d2d discModel ) throws Exception
  {
    final List<IFlowRelationship> parametersToRemove = new ArrayList<>();
    if( element instanceof IElement1D )
    {
      final IElement1D element1D = (IElement1D)element;

      final Set<IFlowRelationship> flowRelsOfElement = FlowRelationUtilitites.findBuildingElements1D( element1D, lFlowRelCollection );

      for( final IFlowRelationship flowRel : flowRelsOfElement )
      {
        final IFE1D2DElement[] elementsOfFlowRelArray = FlowRelationUtilitites.findElementsForFlowRelation( flowRel, discModel );
        final Collection<IFE1D2DElement> elementsOfFlowRel = new ArrayList<>( Arrays.asList( elementsOfFlowRelArray ) );

        elementsOfFlowRel.removeAll( element1DtoRemove );
        if( elementsOfFlowRel.size() == 0 )
          parametersToRemove.add( flowRel );
      }
    }

    return parametersToRemove.toArray( new IFlowRelationship[parametersToRemove.size()] );
  }

  public static IStatus deleteSelectedFeContiLines( final IMapPanel mapPanel )
  {
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final EasyFeatureWrapper[] selected = selectionManager.getAllFeatures();
    if( selected.length == 0 )
    {
      final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.10" ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message );
    }

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.11" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.12" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return Status.OK_STATUS;

    try
    {
      // to allow continuity line to be deleted, no boundary conditions cannot be positioned on that line
      // also, this line cannot be a part of any transition or junction element
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
      final IFlowRelationshipModel flowRelationshipModel = dataProvider.getModel( IFlowRelationshipModel.class.getName() );

      final IFE1D2DComplexElement<IFENetItem>[] complexElements = discretisationModel.getComplexElements();
      for( final IFE1D2DComplexElement complexElement : complexElements )
      {
        if( complexElement instanceof ITransitionElement )
        {
          final ITransitionElement transitionElement = (ITransitionElement)complexElement;
          final IFELine[] continuityLines = transitionElement.getElements();
          for( final IFELine line : continuityLines )
          {
            for( final EasyFeatureWrapper element : selected )
              if( line.getId().equals( element.getFeature().getId() ) )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFEContlineWidget.24" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFEContlineWidget.25" ) ); //$NON-NLS-1$ //$NON-NLS-2$
                selectionManager.clear();
                return Status.OK_STATUS;
              }
          }
        }
        if( complexElement instanceof IJunctionElement )
        {
          final IJunctionElement junctionElement = (IJunctionElement)complexElement;
          final IFELine[] continuityLines = junctionElement.getElements();
          for( final IFELine line : continuityLines )
          {
            for( final EasyFeatureWrapper element : selected )
              if( line.getId().equals( element.getFeature().getId() ) )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFEContlineWidget.26" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFEContlineWidget.27" ) ); //$NON-NLS-1$ //$NON-NLS-2$
                selectionManager.clear();
                return Status.OK_STATUS;
              }
          }
        }
      }
      final CompositeCommand compositeCommand = new CompositeCommand( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.13" ) ); //$NON-NLS-1$

      // check for boundary conditions on the continuity lines
      final IFeatureBindingCollection<IFlowRelationship> wrappedList = flowRelationshipModel.getFlowRelationsShips();
      for( final IFlowRelationship flowRelationship : wrappedList )
      {
        final IBoundaryCondition bc = (IBoundaryCondition)flowRelationship;
        if( bc != null )
        {
          final String parentElementID = bc.getParentElementID();
          for( final EasyFeatureWrapper element : selected )
            if( element.getFeature().getId().equals( parentElementID ) )
            {
              SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFEContlineWidget.28" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFEContlineWidget.29" ) ); //$NON-NLS-1$ //$NON-NLS-2$
              selectionManager.clear();
              return Status.OK_STATUS;
            }
        }
      }

      // if this code is reached you can delete the features.
      for( final EasyFeatureWrapper element : selected )
      {
        final Feature feature = element.getFeature();
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), new QName[] { IContinuityLine1D.QNAME, IContinuityLine2D.QNAME } ) )
        {
          selectionManager.changeSelection( new Feature[] { feature }, new EasyFeatureWrapper[] {} );
          final DeleteFeatureCommand command = new DeleteFeatureCommand( feature );
          compositeCommand.addCommand( command );
        }
      }

      final CommandableWorkspace workspace = selected[0].getWorkspace();
      workspace.postCommand( compositeCommand );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( e.getMessage(), e );
    }
    return Status.OK_STATUS;
  }
}
