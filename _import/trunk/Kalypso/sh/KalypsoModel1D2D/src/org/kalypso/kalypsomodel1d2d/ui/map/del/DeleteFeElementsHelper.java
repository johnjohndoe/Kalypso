/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteElement1DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENode;
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
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Thomas Jung
 * 
 */
public class DeleteFeElementsHelper
{

  @SuppressWarnings("unchecked")
  public static IStatus deleteSelectedFeElements( IMapPanel mapPanel )
  {
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final EasyFeatureWrapper[] selected = selectionManager.getAllFeatures();
    if( selected.length == 0 )
      return StatusUtilities.createErrorStatus( "Fe-Elemente löschen fehlgeschlagen. Keine Elemente selektiert." );

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( "Objekte löschen", "Selektierte Objekte werden gelöscht. Sind Sie sicher?" ) )
      return Status.OK_STATUS;

    selectionManager.clear();

    try
    {
      // to be allowed to delete the 2D element, no continuity line cannot be positioned on that element
      // to be allowed to delete the 1D element, that element cannot be the last one that touches some continuity line
      final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
      final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );

      if( discretisationModel == null )
        throw new RuntimeException( "Could not found model1d2d" );

      // make a list of all the nodes contained by all the continuity lines
      final List<IFENode> clNodes = new ArrayList<IFENode>();
      final IFeatureWrapperCollection<IFELine> continuityLines = discretisationModel.getContinuityLines();
      for( final IFELine line : continuityLines )
        clNodes.addAll( line.getNodes() ); // usually lines are not overlapped so there is no need to check if some of
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
          throw new IllegalArgumentException( "All easy features in selected must be non null" );
        final IFE1D2DElement element = (IFE1D2DElement) easyFeatureWrapper.getFeature().getAdapter( IFE1D2DElement.class );
        final List<IFE1D2DNode> nodes = element.getNodes();
        for( final IFE1D2DNode node : nodes )
          if( clNodes.contains( node ) )
          {
            if( element instanceof IElement2D )
            {
              SWT_AWT_Utilities.showSwtMessageBoxInformation( "Löschen nicht möglich", "Mindestens eines der selektierten 2D-Elemente ist Teil einer Kontinuitätslinie. Bitte löschen Sie zuerst die Kontinuitätslinie." );
              return Status.OK_STATUS;
            }
            if( element instanceof IElement1D )
            {
              final IFeatureWrapperCollection containers = node.getContainers();
              int numberOfEdgeContainers = 0;
              for( final Object container : containers )
                if( container instanceof IFE1D2DEdge ) // container can be also a line
                  numberOfEdgeContainers++;
              if( numberOfEdgeContainers < 2 )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( "Löschen nicht möglich", "Mindestens eines der selektierten 1D-Elemente ist Teil einer Kontinuitätslinie. Bitte löschen Sie zuerst die Kontinuitätslinie." );
                return Status.OK_STATUS;
              }
            }
          }
      }

      final IKalypsoFeatureTheme featureTheme = UtilMap.findEditableTheme( mapPanel.getMapModell(), IFE1D2DElement.QNAME );
      final Set<Feature> changedFeatureList = new HashSet<Feature>();

      for( final EasyFeatureWrapper easyFeatureWrapper : selected )
      {
        final Feature feature = easyFeatureWrapper.getFeature();
        if( feature != null )
        {
          final IDiscrModel1d2dChangeCommand deleteCmd = DeleteCmdFactory.createDeleteCmd( feature, discretisationModel );

          final CommandableWorkspace workspace = featureTheme.getWorkspace();
          workspace.postCommand( deleteCmd );

          if( deleteCmd instanceof DeletePolyElementCmd )
            changedFeatureList.addAll( ((DeletePolyElementCmd) deleteCmd).getChangedFeatureList() );
          else if( deleteCmd instanceof DeleteElement1DCmd )
            changedFeatureList.addAll( ((DeleteElement1DCmd) deleteCmd).getChangedFeatureList() );
        }
      }

      Feature distFeature = discretisationModel.getFeature();

      Feature[] deletedFeatures = changedFeatureList.toArray( new Feature[changedFeatureList.size()] );
      final GMLWorkspace discWorkspace = discretisationModel.getFeature().getWorkspace();
      FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( discWorkspace, distFeature, deletedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
      discWorkspace.fireModellEvent( event );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Fe-Elemente löschen fehlgeschlagen." );
    }
    return Status.OK_STATUS;
  }

  @SuppressWarnings("unchecked")
  public static IStatus deleteSelectedFeContiLines( final IMapPanel mapPanel )
  {
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final EasyFeatureWrapper[] selected = selectionManager.getAllFeatures();
    if( selected.length == 0 )
      return StatusUtilities.createErrorStatus( "Kontinuitätslinien löschen fehlgeschlagen. Keine Objekte selektiert." );

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( "Objekte löschen", "Selektierte Objekte werden gelöscht. Sind Sie sicher?" ) )
      return Status.OK_STATUS;

    try
    {
      // to allow continuity line to be deleted, no boundary conditions cannot be positioned on that line
      // also, this line cannot be a part of any transition or junction element
      final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
      final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );
      final IFlowRelationshipModel flowRelationshipModel = dataProvider.getModel( IFlowRelationshipModel.class );

      final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = discretisationModel.getComplexElements();
      for( final IFE1D2DComplexElement complexElement : complexElements )
      {
        if( complexElement instanceof ITransitionElement )
        {
          final ITransitionElement transitionElement = (ITransitionElement) complexElement;
          final List<IFELine> continuityLines = transitionElement.getContinuityLines();
          for( final IFELine line : continuityLines )
          {
            for( final EasyFeatureWrapper element : selected )
              if( line.getGmlID().equals( element.getFeature().getId() ) )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "DeleteFEContlineWidget.4" ), Messages.getString( "DeleteFEContlineWidget.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
                selectionManager.clear();
                return Status.OK_STATUS;
              }
          }
        }
        if( complexElement instanceof IJunctionElement )
        {
          final IJunctionElement junctionElement = (IJunctionElement) complexElement;
          final List<IFELine> continuityLines = junctionElement.getContinuityLines();
          for( final IFELine line : continuityLines )
          {
            for( final EasyFeatureWrapper element : selected )
              if( line.getGmlID().equals( element.getFeature().getId() ) )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "DeleteFEContlineWidget.6" ), Messages.getString( "DeleteFEContlineWidget.7" ) ); //$NON-NLS-1$ //$NON-NLS-2$
                selectionManager.clear();
                return Status.OK_STATUS;
              }
          }
        }
      }
      final CompositeCommand compositeCommand = new CompositeCommand( "Objekte löschen" );

      // check for boundary conditions on the continuity lines
      final FeatureList wrappedList = flowRelationshipModel.getWrappedList();
      for( final Object object : wrappedList )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) ((Feature) object).getAdapter( IBoundaryCondition.class );
        if( bc != null )
        {
          final String parentElementID = bc.getParentElementID();
          for( final EasyFeatureWrapper element : selected )
            if( element.getFeature().getId().equals( parentElementID ) )
            {
              SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "DeleteFEContlineWidget.8" ), Messages.getString( "DeleteFEContlineWidget.9" ) ); //$NON-NLS-1$ //$NON-NLS-2$
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
