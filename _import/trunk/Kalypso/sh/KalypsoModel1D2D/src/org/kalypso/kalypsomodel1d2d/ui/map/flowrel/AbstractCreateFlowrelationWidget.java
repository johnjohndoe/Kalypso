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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.HashMap;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipCollection;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.LineString;

/**
 * @author Gernot Belger
 */
public abstract class AbstractCreateFlowrelationWidget extends AbstractWidget
{
  private final int m_grabRadius = 20;

  private IFlowRelationshipCollection m_flowRelCollection = null;

  private IKalypsoFeatureTheme m_flowTheme = null;

  private IFEDiscretisationModel1d2d m_discModel = null;

  /* The current element (node, contiline, 1delement, ...) of the disc-model under the cursor. */
  private IFeatureWrapper2 m_modelElement = null;

  private IFlowRelationship m_existingFlowRelation;

  private final QName m_qnameToCreate;

  public AbstractCreateFlowrelationWidget( final String name, final String tooltip, final QName qnameToCreate )
  {
    super( name, tooltip );

    m_qnameToCreate = qnameToCreate;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  private void reinit( )
  {
    m_flowRelCollection = null;

    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    mapPanel.setMessage( "Klicken Sie in die Karte um einen Parameter hinzuzufügen." );

    m_flowTheme = UtilMap.findEditableTheme( mapModell, m_qnameToCreate );
    if( m_flowTheme == null )
      m_flowTheme = UtilMap.findEditableTheme( mapModell, IFlowRelationship.QNAME );

    m_discModel = UtilMap.findFEModelTheme( mapModell );
    if( m_flowTheme == null || m_discModel == null )
      return;

    final FeatureList featureList = m_flowTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_flowRelCollection = (IFlowRelationshipCollection) parentFeature.getAdapter( IFlowRelationshipCollection.class );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GM_Point currentPos = MapUtilities.transform( mapPanel, p );

    /* Grab next node */
    if( m_discModel == null )
    {
      if( m_modelElement != null )
      {
        m_modelElement = null;

        mapPanel.repaint();
      }
      return;
    }

    final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPos, m_grabRadius * 2 );
    m_modelElement = findModelElementFromCurrentPosition( m_discModel, currentPos, grabDistance );

    /* Node has already a flow relation? */
    m_existingFlowRelation = null;
    if( m_flowRelCollection == null || m_modelElement == null )
    {
      mapPanel.repaint();

      return;
    }

    if( isConsidered( m_modelElement ) )
    {
      final GM_Position flowPosition = getFlowPositionFromElement( m_modelElement );
      if( flowPosition != null )
        m_existingFlowRelation = m_flowRelCollection.findFlowrelationship( flowPosition, 0.0 );
    }

    mapPanel.repaint();
  }

  private GM_Position getFlowPositionFromElement( final IFeatureWrapper2 modelElement )
  {
    try
    {
      /* Node: return its position */
      if( modelElement instanceof IFE1D2DNode )
      {
        final GM_Point point = ((IFE1D2DNode) modelElement).getPoint();
        if( point != null )
          return point.getPosition();
      }
      /* ContinuityLine: return middle of line */
      else if( modelElement instanceof IFE1D2DContinuityLine )
      {
        final IFE1D2DContinuityLine contiLine = (IFE1D2DContinuityLine) modelElement;
        final GM_Curve line = (GM_Curve) contiLine.recalculateElementGeometry();
        if( line != null )
        {
          final LineString jtsLine = (LineString) JTSAdapter.export( line );
          final com.vividsolutions.jts.geom.Point point = JTSUtilities.pointOnLinePercent( jtsLine, 50 );
          return JTSAdapter.wrap( point.getCoordinate() );
        }
      }
      else if( modelElement instanceof IPolyElement )
      {
        final IPolyElement polyElement = (IPolyElement) modelElement;
        final GM_Surface surface = (GM_Surface) polyElement.recalculateElementGeometry();
        return surface.getCentroid().getPosition();
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    catch( Throwable th )
    {
      th.printStackTrace();
    }

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( !isConsidered( m_modelElement ) )
      return;

    try
    {
      final int smallRect = 10;
      /* Node: return its position */
      if( m_modelElement instanceof IFE1D2DNode )
      {
        final GM_Point point = ((IFE1D2DNode) m_modelElement).getPoint();
        final Point nodePoint = MapUtilities.retransform( getMapPanel(), point );
        g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
        if( m_existingFlowRelation != null )
          g.fillRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
      }
      /* ContinuityLine: return middle of line */
      else if( m_modelElement instanceof IFE1D2DContinuityLine )
      {
        final IFE1D2DContinuityLine contiLine = (IFE1D2DContinuityLine) m_modelElement;
        final GM_Curve line = (GM_Curve) contiLine.recalculateElementGeometry();

        final LineSymbolizer symb = new LineSymbolizer_Impl();
        final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
        stroke.setWidth( 3 );
        stroke.setStroke( new Color( 255, 0, 0 ) );
        symb.setStroke( stroke );

        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( m_modelElement.getWrappedFeature(), line, symb );
        de.paint( g, getMapPanel().getProjection() );
      }
      else if( m_modelElement instanceof IPolyElement )
      {
        final IPolyElement polyElement = (IPolyElement) m_modelElement;
        final GM_Surface surface = (GM_Surface) polyElement.recalculateElementGeometry();

        final PolygonSymbolizer symb = new PolygonSymbolizer_Impl();
        final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
        stroke.setWidth( 3 );
        stroke.setStroke( new Color( 255, 0, 0 ) );
        symb.setStroke( stroke );

        final DisplayElement de = DisplayElementFactory.buildPolygonDisplayElement( m_modelElement.getWrappedFeature(), surface, symb );
        de.paint( g, getMapPanel().getProjection() );
      }
    }
    catch( final IncompatibleGeometryTypeException e )
    {
      // should never happen
      e.printStackTrace();
    }
    catch( GM_Exception e )
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

    final String problemMessage;
    if( m_modelElement == null )
      // problemMessage = "Kein FE-Knoten in der Nähe. Parameter können nur an Knoten hinzugefügt werden.";
      // TODO: provider nice common error message
      return;
    else if( m_existingFlowRelation != null )
      // problemMessage = "Hier ist bereits Dieser Knoten besitzt bereits Parameter.";
      // TODO do we need a nice message?
      problemMessage = null;
    else
      problemMessage = null;

    if( problemMessage != null )
    {
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          final Shell shell = display.getActiveShell();
          MessageDialog.openWarning( shell, getName(), problemMessage );
        }
      } );
    }

    /* Check preconditions */
    if( m_modelElement == null )
      return;
    if( m_flowRelCollection == null )
      return;

    final CommandableWorkspace workspace = m_flowTheme.getWorkspace();

    if( m_existingFlowRelation == null )
    {
      /* Create flow relation at position */
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          final Feature parentFeature = m_flowRelCollection.getWrappedFeature();
          final IRelationType parentRelation = m_flowRelCollection.getWrappedList().getParentFeatureTypeProperty();
          final IFlowRelationship flowRel = createNewFeature( workspace, parentFeature, parentRelation );
          if( flowRel == null )
          {
            getMapPanel().repaint();
            return;
          }

          final GM_Position flowPositionFromElement = getFlowPositionFromElement( m_modelElement );
          final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
          flowRel.setPosition( GeometryFactory.createGM_Point( flowPositionFromElement, crs ) );

          /* Post it as an command */
          final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
          final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, flowRel.getWrappedFeature(), selectionManager, true, true );
          try
          {
            workspace.postCommand( command );
          }
          catch( final Throwable e )
          {
            final IStatus status = StatusUtilities.statusFromThrowable( e );
            display.asyncExec( new Runnable()
            {
              public void run( )
              {
                final Shell shell = display.getActiveShell();
                ErrorDialog.openError( shell, getName(), "Fehler beim Hinzufügen eines Parameters", status );
              }
            } );
          }
          getMapPanel().repaint();
          try
          {
            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE );
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
    else
    {
      /* Just select the existing element */
      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();

      final Feature featureToSelect = m_existingFlowRelation.getWrappedFeature();
      final EasyFeatureWrapper easyToSelect = new EasyFeatureWrapper( m_flowTheme.getWorkspace(), featureToSelect, featureToSelect.getParent(), featureToSelect.getParentRelation() );

      final Feature[] featuresToRemove = FeatureSelectionHelper.getFeatures( selectionManager );
      selectionManager.changeSelection( featuresToRemove, new EasyFeatureWrapper[] { easyToSelect } );

      /* Open the feature view in order to show the newly created parameters */
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          try
          {
            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE );
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
  }

  /** Overwrite to let this widget consider other 1d2d-element than nodes. */
  protected boolean isConsidered( final IFeatureWrapper2 modelElement )
  {
    return modelElement instanceof IFE1D2DNode;
  }

  /**
   * Really create the new object.
   * 
   * @return The new object, if null, nothing happens..
   */
  protected abstract IFlowRelationship createNewFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation );

  /**
   * @param grabDistance
   *          The grab distance in world (=geo) coordinates.
   */
  protected abstract IFeatureWrapper2 findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance );
}
