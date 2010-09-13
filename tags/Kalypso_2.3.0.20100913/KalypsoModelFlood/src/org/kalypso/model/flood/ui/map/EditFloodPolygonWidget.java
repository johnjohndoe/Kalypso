package org.kalypso.model.flood.ui.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;

import javax.xml.namespace.QName;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;

/**
 * @author Gernot Belger
 */
public class EditFloodPolygonWidget extends AbstractDelegateWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  public EditFloodPolygonWidget( )
  {
    super( Messages.getString("org.kalypso.model.flood.ui.map.EditFloodPolygonWidget.0"), Messages.getString("org.kalypso.model.flood.ui.map.EditFloodPolygonWidget.1"), new SelectFeatureWidget( "", "", new QName[] { IFloodPolygon.QNAME }, IFloodPolygon.QNAME_PROP_AREA ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.model.flood.ui.map.EditFloodPolygonWidget.4") ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.model.flood.ui.map.EditFloodPolygonWidget.5") + delegateTooltip ); //$NON-NLS-1$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.IMapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

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
          // final IStatus status = StatusUtilities.statusFromThrowable( pie );
          // KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
          pie.printStackTrace();
        }
      }
    } );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    /* Deselect all */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();

    super.finish();
  }

}
