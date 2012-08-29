/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/

package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.Create2dElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * FIXME: this is rubbish -> rather reuse code of Create2DElementWidget instead...<br/>
 * FIXME: makes no sense at all:why create a new 2d element at all? just put the 2d relation on an exisint one and thats
 * it..!
 *
 *
 * @author Gernot Belger
 * @author Thomas Jung
 * @author ig
 */
public class CreateFE2DWeirWidget extends AbstractCreateFlowrelationWidget
{
  private ElementGeometryBuilder m_builder = null;

  private IKalypsoFeatureTheme m_nodeTheme;

  private PointSnapper m_pointSnapper;

  private Point m_currentMapPoint;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private IMapPanel m_mapPanel;

  private Feature m_newParentFeature = null;

  private boolean m_warning;

  public CreateFE2DWeirWidget( )
  {
    super( "CreateFE2DWeirWidget (Constructor): " + Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget.1" ), IWeirFlowRelation2D.QNAME ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    final IFEDiscretisationModel1d2d discModel = UtilMap.findFEModelTheme( mapPanel );
    // we must have the node theme. First node theme gets it
    m_nodeTheme = UtilMap.findEditableTheme( mapPanel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    m_pointSnapper = new PointSnapper( discModel, mapPanel );
    reinit();
  }

  @Override
  public void finish( )
  {
    super.finish();

    getMapPanel().setMessage( "" ); //$NON-NLS-1$
    getMapPanel().setCursor( Cursor.getDefaultCursor() );
  }

  protected void reinit( )
  {
    m_builder = null;
    m_newParentFeature = null;
    if( m_nodeTheme != null )
    {
      m_builder = new ElementGeometryBuilder( 0, m_nodeTheme );
      initMembers();
    }
  }

  public final IFlowRelationshipModel getFlowRelCollection( )
  {
    return m_flowRelCollection;
  }

  public final IFEDiscretisationModel1d2d getDiscModel( )
  {
    return m_discModel;
  }

  public final IKalypsoFeatureTheme getFlowTheme( )
  {
    return m_flowTheme;
  }

  private void initMembers( )
  {
    m_mapPanel = getMapPanel();
    if( m_mapPanel == null )
      return;

    if( m_builder != null )
    {
      m_flowTheme = UtilMap.findEditableTheme( m_mapPanel, IFlowRelation2D.QNAME );
      if( m_flowTheme == null )
        m_flowTheme = UtilMap.findEditableTheme( m_mapPanel, IFlowRelationship.QNAME );

      m_discModel = UtilMap.findFEModelTheme( m_mapPanel );
      if( m_flowTheme != null && m_discModel != null )
      {
        final FeatureList featureList = m_flowTheme.getFeatureList();
        final Feature parentFeature = featureList.getOwner();
        m_flowRelCollection = (IFlowRelationshipModel) parentFeature.getAdapter( IFlowRelationshipModel.class );
      }
    }
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_currentMapPoint != null )
    {
      if( m_builder != null )
      {
        m_builder.paint( g, getMapPanel().getProjection(), m_currentMapPoint );

        if( m_pointSnapper != null )
          m_pointSnapper.paint( g );
      }
    }

    super.paint( g );

    final Rectangle bounds = mapPanel.getScreenBounds();
    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.8" ) ); //$NON-NLS-1$
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    if( m_warning == true )
      m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 80 ), g, bounds );

    if( m_pointSnapper != null )
    {
      final IFE1D2DNode snapNode = m_pointSnapper.getSnapNode();
      if( snapNode == null )
        return;

      final double z = snapNode.getPoint().getZ();
      if( !Double.isNaN( z ) )
      {
        final String format = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryEditor.9", z ); //$NON-NLS-1$
        getMapPanel().setMessage( format );
      }
    }
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_pointSnapper.activate( false );
  }

  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_pointSnapper.activate( true );
  }

  @Override
  public void keyTyped( final KeyEvent e )
  {
    super.keyTyped( e );
    if( KeyEvent.VK_ESCAPE == e.getKeyChar() )
    {
      reinit();
      repaintMap();
    }
    else if( KeyEvent.VK_BACK_SPACE == e.getKeyChar() || KeyEvent.VK_DELETE == e.getKeyChar() )
    {
      m_builder.removeLast();
      repaintMap();
    }
  }

  @Override
  public void moved( final Point p )
  {
    final Object newNode = checkNewNode( p, false );
    if( newNode instanceof IFE1D2DNode )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    repaintMap();
    super.moved( p );
  }

  /**
   * Return one 2D-Element.
   */
  @Override
  protected Feature findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
  {
    return null;
    // return discModel.find2DElement( currentPos, grabDistance );
  }

  @Override
  public void leftClicked( final Point p )
  {
    final Object newNode = checkNewNode( p, false );

    if( newNode == null )
      return;

    try
    {
      // FIXME: will never happen, because builder point count is set to 0
      Create2dElementCommand command;
      if( newNode instanceof GM_Point )
        command = m_builder.addNode( (GM_Point) newNode );
      else
        command = m_builder.addNode( ((IFE1D2DNode) newNode).getPoint() );

      if( command != null )
      {
        m_nodeTheme.getWorkspace().postCommand( command );

        m_newParentFeature = command.getNewElement();

        if( m_newParentFeature != null )
        {
          setModelElement( m_newParentFeature );
          repaintMap();
          super.leftClicked( p );
          reinit();
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      repaintMap();
    }
  }

  /**
   * TODO: change to right-clicked: BUT!: at the moment the context menu is opened, so the framework must know whether
   * this widget is editing something at the moment or not
   *
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    // Check again, else we would need a state flag, whether leftClicked was OK
    if( checkNewNode( p ) == null )
      return;

    if( m_builder.getNumberOfNodes() < 4 )
      return;

    if( m_builder.getNumberOfNodes() % 2 != 0 )
      return;

    try
    {
      final Create2dElementCommand command = m_builder.finish();
      if( command != null )
      {
        m_nodeTheme.getWorkspace().postCommand( command );
        m_newParentFeature = command.getNewElement();

        if( m_newParentFeature != null )
        {
          setModelElement( m_newParentFeature );
          super.leftClicked( p );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      reinit();
      repaintMap();
    }
  }

  private Object checkNewNode( final Point p )
  {
    return checkNewNode( p, true );
  }

  private Object checkNewNode( final Point p, final boolean pBoolFinalPoint )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_builder == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    final IFE1D2DNode snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );
    final Object newNode = snapNode == null ? currentPoint : snapNode;

    IStatus status;
    if( newNode instanceof GM_Point )
      status = m_builder.checkNewNode( (GM_Point) newNode, pBoolFinalPoint );
    else
      status = m_builder.checkNewNode( ((IFE1D2DNode) newNode).getPoint(), pBoolFinalPoint );

    if( status.isOK() )
      m_warning = false;
    else
    {
      if( status.getMessage().equals( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.4" ) ) ) //$NON-NLS-1$
      {
        reinit();
        repaintMap();
      }
      m_warning = true;
      m_warningRenderer.setTooltip( status.getMessage() );
    }

    if( status.isOK() )
      return newNode;

    return null;
  }

  @Override
  protected IFlowRelationship createNewFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation, final Feature modelElement )
  {
    final IFeatureType newFT = GMLSchemaUtilities.getFeatureTypeQuiet( IWeirFlowRelation2D.QNAME );
    final Feature newFeature = workspace.createFeature( parentFeature, parentRelation, newFT, -1 );
    final IWeirFlowRelation2D weirRelation = (IWeirFlowRelation2D) newFeature.getAdapter( IWeirFlowRelation2D.class );
    /* Call getObservation once to initialize it */
    weirRelation.getBuildingObservation();
    return weirRelation;
  }
}
