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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.CssParameter;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;

/**
 * @author ig
 */
//public class EditWeir2DFlowrelationWidget extends AbstractWidget
public class EditWeir2DFlowrelationWidget extends AbstractDelegateWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private IKalypsoFeatureTheme m_flowTheme = null;

  protected IFEDiscretisationModel1d2d m_discModel = null;

  IFlowRelationshipModel m_flowRelModel;

  int m_grabRadius = 10;

  private Feature m_modelElement;

  IKalypsoFeatureTheme[] m_themes;

  public EditWeir2DFlowrelationWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.1" ), new SelectFeatureWidget( "", "", new QName[] { IFlowRelation2D.QNAME //, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        , IBuildingFlowRelation2D.QNAME }, IFlowRelationship.QNAME_PROP_POSITION ) );

    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.4" ) ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
  }

  // public EditWeir2DFlowrelationWidget( )
  // {
  //    super( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter2dWidget.0"), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter2dWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  //
  //    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter2dWidget.1" ) ); //$NON-NLS-1$
  // m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
  // }

  // /**
  // * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
  // */
  // @Override
  // public void paint( final Graphics g )
  // {
  // super.paint( g );
  //
  // final IMapPanel mapPanel = getMapPanel();
  // if( mapPanel != null )
  // {
  // final Rectangle bounds = mapPanel.getScreenBounds();
  // final String delegateTooltip = getDelegate().getToolTip();
  //
  //      m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.5") + delegateTooltip ); //$NON-NLS-1$
  //
  // m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
  // }
  // }

  @Override
  public void paint( final Graphics g )
  {
    if( m_modelElement == null )
      return;

    try
    {
      /* Node: return its position */
      if( m_modelElement instanceof IPolyElement )
      {
        final IPolyElement polyElement = (IPolyElement)m_modelElement;
        final GM_Polygon surface = polyElement.getGeometry();

        final PolygonSymbolizer symb = new PolygonSymbolizer_Impl();
        final Stroke stroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );
        stroke.setWidth( 3 );
        stroke.setStroke( new Color( 255, 0, 0 ) );
        symb.setStroke( stroke );

        final DisplayElement de = DisplayElementFactory.buildPolygonDisplayElement( m_modelElement, surface, symb );
        de.paint( g, getMapPanel().getProjection(), new NullProgressMonitor() );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();

    if( m_modelElement == null || m_flowRelModel == null )
      return;

    final IMapPanel mapPanel = getMapPanel();
    /* Edit flow relation at position */
    display.asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        final GM_Point currentPos = MapUtilities.transform( mapPanel, p );

        final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPos, m_grabRadius );
        final Feature lFoundPolyElement = findModelElementFromCurrentPosition( m_discModel, currentPos, grabDistance );

        if( lFoundPolyElement == null )
        {
          mapPanel.repaintMap();
          return;
        }
        final Feature lFoundFlowRel = FlowRelationUtilitites.findBuildingElement2D( (IPolyElement)lFoundPolyElement, m_flowRelModel );
        final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
        selectionManager.clear();

        final List<Feature> lListToSelect = new ArrayList<>();
        lListToSelect.add( lFoundFlowRel );

        changeSelection( selectionManager, lListToSelect, m_themes, false, false );
        mapPanel.repaintMap();
        try
        {
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE ); //$NON-NLS-1$
        }
        catch( final Throwable pie )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( pie );
          KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
          pie.printStackTrace();
        }
      }
    } );
  }

  public static void changeSelection( final IFeatureSelectionManager selectionManager, final List<Feature> selectedFeatures, final IKalypsoFeatureTheme[] themes, final boolean add, final boolean toggle )
  {
    if( selectedFeatures.size() == 0 )
      selectionManager.clear();

    final List<Feature> toRemove = new ArrayList<>();
    final List<EasyFeatureWrapper> toAdd = new ArrayList<>();

    for( final IKalypsoFeatureTheme theme : themes )
    {
      /* consider the selection modes */
      final CommandableWorkspace workspace = theme.getWorkspace();

      for( final Feature feature : selectedFeatures )
      {
        if( add )
        {
          if( !selectionManager.isSelected( feature ) )
            toAdd.add( new EasyFeatureWrapper( workspace, feature ) );
        }
        else if( toggle )
        {
          if( selectionManager.isSelected( feature ) )
            toRemove.add( feature );
          else
            toAdd.add( new EasyFeatureWrapper( workspace, feature ) );
        }
        else
          toAdd.add( new EasyFeatureWrapper( workspace, feature ) );
      }
    }

    if( !add && !toggle )
    {
      // REMARK: instead of invoking
      // selectionManager.clear();
      // We add all features to the remove-list; else we get two selection-change events here
      final EasyFeatureWrapper[] allFeatures = selectionManager.getAllFeatures();
      for( final EasyFeatureWrapper feature : allFeatures )
        toRemove.add( feature.getFeature() );
    }

    selectionManager.changeSelection( toRemove.toArray( new Feature[toRemove.size()] ), toAdd.toArray( new EasyFeatureWrapper[toAdd.size()] ) );
    selectionManager.changeSelection( toRemove.toArray( new Feature[toRemove.size()] ), selectionManager.getAllFeatures() );
  }

  @Override
  public void moved( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GM_Point currentPos = MapUtilities.transform( mapPanel, p );

    /* Grab next node */
    if( m_discModel == null )
    {
      if( m_modelElement != null )
      {
        m_modelElement = null;

        mapPanel.repaintMap();
      }
      return;
    }

    final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPos, m_grabRadius );
    m_modelElement = findModelElementFromCurrentPosition( m_discModel, currentPos, grabDistance );

    mapPanel.repaintMap();
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    init( mapPanel );
    /* Open the feature view */
    final Display display = PlatformUI.getWorkbench().getDisplay();
    display.asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        try
        {
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE ); //$NON-NLS-1$
        }
        catch( final Throwable pie )
        {
          pie.printStackTrace();
        }
      }
    } );
  }

  private void init( final IMapPanel mapPanel )
  {
    m_flowTheme = UtilMap.findEditableTheme( mapPanel, IFlowRelationship.QNAME );
    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    if( m_flowTheme == null )
      return;

    final FeatureList featureList = m_flowTheme.getFeatureList();
    final Feature parentFeature = featureList.getOwner();
    m_flowRelModel = (IFlowRelationshipModel)parentFeature.getAdapter( IFlowRelationshipModel.class );

    final IMapModell mapModell = mapPanel.getMapModell();
    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      m_themes = new IKalypsoFeatureTheme[1];
      m_themes[0] = (IKalypsoFeatureTheme)activeTheme;
    }
  }

  // /**
  // * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
  // */
  // @Override
  // public void keyTyped( final KeyEvent e )
  // {
  // if( e.getKeyChar() == '\n' )
  // {
  // e.consume();
  //
  // // final EasyFeatureWrapper[] features = getMapPanel().getSelectionManager().getAllFeatures();
  // // final List<IFlowRelationship> flowRels = new ArrayList<IFlowRelationship>( features.length );
  // // for( final EasyFeatureWrapper feature : features )
  // // {
  // // final IFlowRelationship adapter = (IFlowRelationship) feature.getAdapter( IFlowRelationship.class );
  // // if( adapter != null )
  // // flowRels.add( adapter );
  // // }
  // //
  // // // Force it into swt
  // // final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
  // // final Shell shell = (Shell) service.getCurrentState().getVariable( ISources.ACTIVE_SHELL_NAME );
  // // shell.getDisplay().asyncExec( new Runnable()
  // // {
  // // public void run( )
  // // {
  // // startCalculation( shell, flowRels.toArray( new IFlowRelationship[flowRels.size()] ) );
  // // }
  // // } );
  // return;
  // }
  // super.keyTyped( e );
  // }

  @Override
  public void finish( )
  {
    /* Deselect all */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();

    super.finish();
  }

  protected Feature findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
  {
    final Feature lFoundElement2d = discModel.find2DElement( currentPos, grabDistance );
    if( lFoundElement2d == null )
    {
      return null;
    }
    final Feature lBuildingExisting = FlowRelationUtilitites.findBuildingElement2D( (IPolyElement)lFoundElement2d, m_flowRelModel );
    if( lBuildingExisting != null )
    {
      return lFoundElement2d;
    }
    else
    {
      return null;
    }
  }
}
