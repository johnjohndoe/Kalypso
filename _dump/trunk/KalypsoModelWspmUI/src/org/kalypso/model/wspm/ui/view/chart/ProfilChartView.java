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
package org.kalypso.model.wspm.ui.view.chart;

import java.awt.Insets;
import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableElement;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum;
import org.kalypso.model.wspm.ui.view.chart.color.IProfilColorSet;

import de.belger.swtchart.ChartCanvas;
import de.belger.swtchart.action.ChartStandardActions;
import de.belger.swtchart.action.SaveChartAsAction;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.axis.IAxisRenderer;
import de.belger.swtchart.axis.TickRenderer;
import de.belger.swtchart.layer.IChartLayer;
import de.belger.swtchart.mouse.AbstractChartPosListener;
import de.belger.swtchart.mouse.IChartPosListener;
import de.belger.swtchart.util.SwitchDelegate;

/**
 * @author belger
 */
public class ProfilChartView extends AbstractProfilView implements IPersistableElement
{
  private final static int AXIS_WIDTH = 2;

  private static final Insets LABEL_INSETS = new Insets( 5, 0, 5, 0 );

  private static final String MEM_LAYER_VIS = "layerVisibility";

  private static final String MEM_ACTION_CHECK = "actionCheckState";

  private static final Insets TICK_INSETS = new Insets( 2, 10, 10, 10 );

  private final static int TICK_LENGTH = 10;

  private ChartStandardActions m_actions;

  protected ChartCanvas m_chart = null;

  private final ColorRegistry m_colorRegistry;

  private AxisRange m_domainRange;

  private final Collection<IChartPosListener> m_poslisteners = new LinkedList<IChartPosListener>();

  private AxisRange m_valueRangeLeft;

  private AxisRange m_valueRangeRight;

  public ProfilChartView( final IProfilEventManager pem, final ProfilViewData viewdata, final ColorRegistry colorRegistry )
  {
    super( pem, viewdata );

    m_colorRegistry = colorRegistry;
  }

  public void addChartPosListener( final IChartPosListener l )
  {
    m_poslisteners.add( l );
  }

  private void addLayer( final IProfilChartLayer layer, final Map<String, Boolean> visibility )
  {
    final Boolean visible = visibility.get( layer.toString() );
    final boolean defaultVisibility = layer.getInitialVisibility();
    m_chart.addLayer( layer, visible == null ? defaultVisibility : visible );
  }

  @SuppressWarnings("boxing")
  protected void createLayer( )
  {
    // get visibility
    final Map<String, Boolean> visibility = new HashMap<String, Boolean>();
    for( final IChartLayer layer : m_chart.getLayers() )
      visibility.put( layer.toString(), m_chart.isVisible( layer ) );

    m_chart.clearLayers();

    final IProfil profil = getProfil();
    if( profil != null )
    {
      final String profiletype = profil.getType();
      final IProfilLayerProvider provider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( profiletype );
      if( provider == null )
        return;

      // call provider
      final IProfilChartLayer[] layers = provider.createLayer( this, profil, getResults(), m_domainRange, m_valueRangeLeft, m_valueRangeRight, m_colorRegistry );
      for( final IProfilChartLayer layer : layers )
        addLayer( layer, visibility );
    }
  }

  /**
   * @see com.bce.profil.ui.view.IProfilView#dispose()
   */
  @Override
  public void dispose( )
  {
    m_poslisteners.clear();

    if( m_chart != null )
      m_chart.dispose();

    // fix by marc, TODO please Gernot/Kim check if this is ok
    if( m_actions != null )
      m_actions.dispose();

    m_chart = null;

    super.dispose();
  }

  /**
   * @see com.bce.profil.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  public Control doCreateControl( final Composite parent, final int style )
  {
    m_chart = new ChartCanvas( parent, style, new Insets( 20, 0, 0, 0 ) );
    m_chart.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    // TODO: move this to layer provider
    // or even better: let layer provider create layers and then retrieve domain/value axes from layers
    m_domainRange = new AxisRange( "", SwitchDelegate.HORIZONTAL, false, 5, 1.0 );
    m_valueRangeLeft = new AxisRange( "", SwitchDelegate.VERTICAL, true, 5, 1.0 );
    m_valueRangeRight = new AxisRange( "", SwitchDelegate.VERTICAL, true, 0, 0.2 );
//    m_domainRange = new AxisRange( "[m]", SwitchDelegate.HORIZONTAL, false, 5, 1.0 );
//    m_valueRangeLeft = new AxisRange( "[m+NN]", SwitchDelegate.VERTICAL, true, 5, 1.0 );
//    m_valueRangeRight = new AxisRange( "[KS]", SwitchDelegate.VERTICAL, true, 0, 0.2 );

    final IAxisRenderer domainrenderer = new TickRenderer( m_colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND ), m_colorRegistry.get( IProfilColorSet.COLOUR_AXIS_BACKGROUND ), AXIS_WIDTH, TICK_LENGTH, TICK_INSETS, 3, LABEL_INSETS, null, true );
    final IAxisRenderer leftrenderer = new TickRenderer( m_colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND ), m_colorRegistry.get( IProfilColorSet.COLOUR_AXIS_BACKGROUND ), AXIS_WIDTH, TICK_LENGTH, TICK_INSETS, 3, LABEL_INSETS, null, false );
    final IAxisRenderer rightrenderer = new TickRenderer( m_colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND ), m_colorRegistry.get( IProfilColorSet.COLOUR_AXIS_BACKGROUND ), AXIS_WIDTH, TICK_LENGTH, TICK_INSETS, 3, LABEL_INSETS, null, true );

    m_chart.setAxisRenderer( m_domainRange, domainrenderer );
    m_chart.setAxisRenderer( m_valueRangeLeft, leftrenderer );
    m_chart.setAxisRenderer( m_valueRangeRight, rightrenderer );
    m_chart.setFixAspectRatio( null );
    try
    {
      createLayer();
    }
    catch( final Throwable e )
    {
      // catch everything, because if we get a runtime exception here
      // all goes down the privy...
      e.printStackTrace();
    }

    m_chart.maximize();

    m_chart.addMouseMoveListener( new AbstractChartPosListener( m_domainRange, m_valueRangeLeft )
    {
      public void onPosChanged( final Point2D logpoint, final boolean inScreen )
      {
        firePosChanged( logpoint, inScreen );
      }
    } );

    m_actions = new ChartStandardActions( m_chart, m_domainRange, m_valueRangeLeft );

    return m_chart;
  }

  protected void firePosChanged( final Point2D logpoint, final boolean inScreen )
  {
    for( final IChartPosListener l : m_poslisteners )
      l.onPosChanged( logpoint, inScreen );
  }

  public ChartStandardActions getActions( )
  {
    return m_actions;
  }

  public ChartCanvas getChart( )
  {
    return m_chart;
  }

  /**
   * @see org.eclipse.ui.IPersistableElement#getFactoryId()
   */
  public String getFactoryId( )
  {
    return null;
  }

  /**
   * @see com.bce.profil.ui.view.AbstractProfilView#onProfilViewDataChanged()
   */
  @Override
  public void onProfilViewDataChanged( )
  {
    // redrawChart();
  }

  protected void redrawChart( )
  {
    final ChartCanvas chart = m_chart;
    if( chart != null && !chart.isDisposed() )
    {
      chart.getDisplay().syncExec( new Runnable()
      {

        public void run( )
        {
          chart.repaint();
        }
      } );
    }
  }

  public void removeChartPosListener( final IChartPosListener l )
  {
    m_poslisteners.remove( l );
  }

  public void restoreState( final IMemento memento )
  {
    if( m_chart == null )
      return;

    final Map<String, Boolean> hash = new HashMap<String, Boolean>();
    final IMemento[] layerChildren = memento.getChildren( MEM_LAYER_VIS );
    for( final IMemento layermem : layerChildren )
    {
      final String name = layermem.getID();
      final String textData = layermem.getTextData();
      final Boolean visib = Boolean.valueOf( textData );
      hash.put( name, visib );
    }

    for( final IChartLayer layer : m_chart.getLayers() )
    {
      final Boolean visib = hash.get( layer.toString() );
      if( visib != null )
        m_chart.setVisible( layer, visib );
    }

    m_actions.restoreState( memento, MEM_ACTION_CHECK );
  }

  /**
   * @see org.eclipse.ui.IPersistableElement#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    if( m_chart == null )
      return;

    for( final IChartLayer layer : m_chart.getLayers() )
    {
      final IMemento layermem = memento.createChild( MEM_LAYER_VIS, layer.toString() );
      layermem.putTextData( "" + m_chart.isVisible( layer ) );
    }

    m_actions.saveState( memento, MEM_ACTION_CHECK );
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    m_chart.getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        if( hint.isPointPropertiesChanged() || hint.isBuildingChanged() )
        {
          createLayer();
          return;
        }
        if( hint.isPointsChanged() || hint.isPointValuesChanged() || hint.isBuildingDataChanged() || hint.isDeviderMoved() || hint.isProfilPropertyChanged() || hint.isActivePointChanged() )
        {
          for( final IChartLayer layer : m_chart.getLayers() )
          {
            if( layer instanceof IProfilChartLayer )
              ((IProfilChartLayer) layer).onProfilChanged( hint, changes );
          }
        }
        redrawChart();
      }
    } );
  }

  public void runChartAction( final ProfilChartActionsEnum chartAction )
  {
    if( m_chart != null && !m_chart.isDisposed() )
    {
      switch( chartAction )
      {
        case MAXIMIZE:
          m_chart.maximize();
          break;
        case FIX_RATIO_0:
          m_chart.setFixAspectRatio( null );
          m_chart.repaint();
          break;
        case FIX_RATIO_1:
          m_chart.setFixAspectRatio( 1.0 );
          m_chart.repaint();
          break;
        case FIX_RATIO_2:
          m_chart.setFixAspectRatio( 2.0 );
          m_chart.repaint();
          break;
        case FIX_RATIO_3:
          m_chart.setFixAspectRatio( 10.0 );
          m_chart.repaint();
          break;

        case ZOOM_IN:
          m_actions.getAction( ChartStandardActions.Action.ZOOM_IN ).setChecked( true );
          break;
        case ZOOM_OUT:
          m_actions.getAction( ChartStandardActions.Action.ZOOM_OUT ).setChecked( true );
          break;
        case PAN:
          m_actions.getAction( ChartStandardActions.Action.PAN ).setChecked( true );
          break;

        case EDIT:
        {
          final IAction editAction = m_actions.getAction( ChartStandardActions.Action.EDIT );
          editAction.setChecked( !editAction.isChecked() );
        }
          break;

        case EXPORT_IMAGE:
          saveChartAsImage( m_chart );
          break;

        case FIX_RATIO:
          break;
      }
    }
  }

  private void saveChartAsImage( final ChartCanvas chart )
  {
    new SaveChartAsAction( chart ).run();
  }
}
