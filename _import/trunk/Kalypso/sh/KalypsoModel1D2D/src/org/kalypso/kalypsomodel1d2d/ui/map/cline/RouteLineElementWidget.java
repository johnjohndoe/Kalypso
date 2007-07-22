package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ILineElement;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.RouteLineElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provides a widget functionality to route a line element. The user will draw a line string and double click at the and
 * to route the line element
 * 
 * @author Patrice Congo
 */
@SuppressWarnings("unchecked")
public class RouteLineElementWidget<T extends ILineElement> extends AbstractWidget
{
  private Point m_currentPoint = null;

  private LineGeometryBuilder m_builder = null;

  /**
   * QName of the element to create
   */
  final private QName lineElementQName;

  /**
   * Class to which the created line element should be adapted
   */
  final private Class<T> adapterTargetClass;

// /**
// * The latest routed element
// */
// T lastRouted;
//  

  /**
   * Creates a route line widget.
   * 
   * @param name
   *            the name of the new widget
   * @param tooltip
   *            the tool tip associated with the new widget
   * @param adapterTargetClass
   *            the target adapter class for the line element to be created
   * @param lineElementQName
   *            the q-name of the line element to be created
   */
  public RouteLineElementWidget( final String name, final String tooltip, final Class<T> adapterTargetClass, final QName lineElementQName )
  {
    super( name, tooltip );
    Assert.throwIAEOnNullParam( adapterTargetClass, "adapterTargetClass" );
    Assert.throwIAEOnNullParam( lineElementQName, "lineElementQName" );
    this.adapterTargetClass = adapterTargetClass;
    this.lineElementQName = lineElementQName;
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

  private final void reinit( )
  {
    m_builder = null;

    final CS_CoordinateSystem targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();
    m_builder = new LineGeometryBuilder( 0, targetCrs );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( KeyEvent e )
  {
    if( KeyEvent.VK_ESCAPE == e.getKeyChar() )
    {
      this.reinit();
      getMapPanel().repaint();
    }
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      if( m_builder != null )
      {
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), m_currentPoint );

        m_builder.addPoint( currentPos );

      }
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      getMapPanel().repaint();

    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    try
    {
      final GM_Curve curve = (GM_Curve) m_builder.finish();

      // validate geometry: doppelte punkte

      final IMapModell mapModell = getMapPanel().getMapModell();
      final IFEDiscretisationModel1d2d model = UtilMap.findFEModelTheme( mapModell );
      final IKalypsoTheme theme = UtilMap.findEditableTheme( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      final CommandableWorkspace workspace = ((IKalypsoFeatureTheme) theme).getWorkspace();

// lastRouted =
// ContinuityLineOps.lineElementFromCurve(
// lineElementQName,
// adapterTargetClass,
// curve,
// model );//contilineFromCurve( curve, model );
// lastRouted.getWrappedFeature().invalidEnvelope();
//      
// final Feature parentFeature = model.getWrappedFeature();
// final IRelationType rt =
// (IRelationType) parentFeature.getFeatureType().getProperty(
// Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS);
// final AddFeatureCommand addElementCommand =
// new AddFeatureCommand(
// workspace, parentFeature, rt, -1,
// lastRouted.getWrappedFeature(), null, true );
// workspace.postCommand( addElementCommand );
      final RouteLineElementCommand command = new RouteLineElementCommand( model, curve, adapterTargetClass, lineElementQName );
      workspace.postCommand( command );
    }
    catch( final Exception e )
    {
// lastRouted = null;
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );

      final IStatus status = StatusUtilities.statusFromThrowable( e );

      final Shell shell = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage().getActivePart().getSite().getShell();
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          ErrorDialog.openError( shell, getName(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.2" ), status ); //$NON-NLS-1$
        }
      } );
    }
    finally
    {
      reinit();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

    if( currentPoint != null )
    {
      if( m_builder != null )
        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    }
  }

// /**
// * To get the last routed line element
// *
// * @return the lastly routed line element by this widget or
// * null if there is no such element
// */
// public T getLastRooted()
// {
// return lastRouted;
// }

}
