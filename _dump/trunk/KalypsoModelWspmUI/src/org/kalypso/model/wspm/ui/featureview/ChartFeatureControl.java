/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.ui.featureview;

import java.net.URL;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.DialogSettings;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypso.util.swt.StatusComposite;
import org.kalypsodeegree.model.feature.Feature;

import de.openali.odysseus.chart.factory.config.ChartConfigurationLoader;
import de.openali.odysseus.chart.factory.config.ChartExtensionLoader;
import de.openali.odysseus.chart.factory.config.ChartFactory;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.util.ChartUtilities;
import de.openali.odysseus.chart.framework.view.impl.ChartComposite;
import de.openali.odysseus.chartconfig.x020.ChartType;

/**
 * @author Gernot Belger
 */
public class ChartFeatureControl extends AbstractFeatureControl implements IFeatureControl
{
  /** These settings are used locally to remember the last selected tab-folder. */
  private final static IDialogSettings SETTINGS = new DialogSettings( "bla" ); //$NON-NLS-1$

  private final static String STR_SETTINGS_TAB = "tabIndex"; //$NON-NLS-1$

  private ChartComposite[] m_charts;

  private final ChartType[] m_chartTypes;

  private final URL m_context;

  private final ChartConfigurationLoader m_ccl;

  private ChartTabItem[] m_chartTabs;

  private final Map<String, Integer> m_commands;

  public ChartFeatureControl( final Feature feature, final IPropertyType ftp, final ChartConfigurationLoader ccl, final ChartType[] chartTypes, final URL context, final Map<String, Integer> commands )
  {
    super( feature, ftp );
    m_ccl = ccl;
    m_commands = commands;

    m_chartTypes = chartTypes;
    m_context = context;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_charts = new ChartComposite[m_chartTypes.length];
    m_chartTabs = new ChartTabItem[m_chartTypes.length];

    if( m_chartTabs.length == 0 )
    {
      final IStatus warningStatus = StatusUtilities.createStatus( IStatus.WARNING, Messages.ChartFeatureControl_2, null );
      final StatusComposite statusComposite = new StatusComposite( parent, SWT.NONE );
      statusComposite.setStatus( warningStatus );
      return statusComposite;
    }

    if( m_chartTabs.length == 1 )
    {
      // REAMRK: we do not tab, if we have only one chart
      m_chartTabs[0] = new ChartTabItem( /* composite */parent, style, m_commands );

      updateControl();

      return m_chartTabs[0];
    }

    final TabFolder folder = new TabFolder( parent, SWT.TOP );

    for( int i = 0; i < m_chartTypes.length; i++ )
    {
      final ChartType chartType = m_chartTypes[i];

      /* The tab item */
      final TabItem item = new TabItem( folder, SWT.NONE );
      item.setText( chartType.getTitle() );
      item.setToolTipText( chartType.getDescription() );

      m_chartTabs[i] = new ChartTabItem( folder, style, m_commands );

      item.setControl( m_chartTabs[i] );
    }

    final String selectedTabStr = SETTINGS.get( STR_SETTINGS_TAB );
    final int selectedTab = selectedTabStr == null ? 0 : Integer.parseInt( selectedTabStr );
    if( selectedTab < folder.getTabList().length )
      folder.setSelection( selectedTab );

    folder.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleFolderSelectionChanged( folder.getSelectionIndex() );
      }
    } );

    updateControl();

    return folder;
  }

  protected void handleFolderSelectionChanged( final int selectionIndex )
  {
    ChartFeatureControl.SETTINGS.put( ChartFeatureControl.STR_SETTINGS_TAB, selectionIndex );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    for( int i = 0; i < m_charts.length; i++ )
    {
      final ChartComposite chart = m_chartTabs[i].getChartComposite();

      // if the chart was previously loaded, it will contain layers - these have to be removed
      ILayerManager lm = chart.getChartModel().getLayerManager();
      IChartLayer[] layers = lm.getLayers();
      for( IChartLayer chartLayer : layers )
      {
        lm.removeLayer( chartLayer );
      }
      ChartFactory.doConfiguration( chart.getChartModel(), m_ccl, m_chartTypes[i], ChartExtensionLoader.getInstance(), m_context );
      ChartUtilities.maximize( chart.getChartModel() );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_chartTabs != null )
    {
      for( final ChartTabItem item : m_chartTabs )
        item.dispose();
    }

    super.dispose();
  }
}
