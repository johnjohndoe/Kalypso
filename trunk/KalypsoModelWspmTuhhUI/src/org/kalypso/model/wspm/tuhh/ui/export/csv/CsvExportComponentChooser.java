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
package org.kalypso.model.wspm.tuhh.ui.export.csv;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.deegree.binding.gml.Definition;
import org.kalypso.deegree.binding.gml.Dictionary;
import org.kalypso.deegree.binding.swe.ItemDefinition;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.om.FeatureComponent;

/**
 * Lets the user choose among all possible available components in a length section.
 * 
 * @author Gernot Belger
 */
public class CsvExportComponentChooser
{
  private static final String SETTINGS_COLUMNS = "selectedColumns"; //$NON-NLS-1$

  private final Collection<IComponent> m_components = new HashSet<IComponent>();

  private IComponent[] m_selectedColumns = new IComponent[0];

  private final IDialogSettings m_settings;

  public CsvExportComponentChooser( final IDialogSettings settings )
  {
    m_settings = settings;
    initComponents( IWspmTuhhConstants.URN_OGC_GML_DICT_KALYPSO_MODEL_WSPM_COMPONENTS );
    initSelection();
  }

  private void initSelection( )
  {
    if( m_settings == null )
      return;

    final String[] ids = m_settings.getArray( SETTINGS_COLUMNS );
    final Collection<IComponent> selectedComponent = new ArrayList<IComponent>();
    for( final IComponent component : m_components )
    {
      if( ArrayUtils.contains( ids, component.getId() ) )
        selectedComponent.add( component );
    }

    m_selectedColumns = selectedComponent.toArray( new IComponent[selectedComponent.size()] );
  }

  private void initComponents( final String urn )
  {
    final Dictionary dictionary = KalypsoCorePlugin.getDefault().getDictionary( urn );
    final Definition[] allDefinitions = dictionary.getAllDefinitions();
    for( final Definition definition : allDefinitions )
    {
      if( definition instanceof ItemDefinition )
        m_components.add( new FeatureComponent( definition, urn ) );
    }
  }

  public Control createControl( final Composite parent )
  {
    final Table table = new Table( parent, SWT.CHECK | SWT.HIDE_SELECTION | SWT.BORDER );
    final CheckboxTableViewer viewer = new CheckboxTableViewer( table );

    viewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object[] checkedElements = viewer.getCheckedElements();
        handleColumnChecked( checkedElements );
      }
    } );

    viewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        final IComponent component = (IComponent) element;
        // FIXME: replace later with getLabel
        return component.getName();
      }
    } );

    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setComparator( new ViewerComparator() );
    viewer.setInput( m_components );
    viewer.setCheckedElements( m_selectedColumns );

    return table;
  }

  protected void handleColumnChecked( final Object[] checkedElements )
  {
    m_selectedColumns = Arrays.castArray( checkedElements, new IComponent[checkedElements.length] );
    final String[] ids = new String[m_selectedColumns.length];
    for( int i = 0; i < ids.length; i++ )
      ids[i] = m_selectedColumns[i].getId();

    if( m_settings != null )
      m_settings.put( SETTINGS_COLUMNS, ids );
  }

  public IComponent[] getSelectedComponents( )
  {
    return m_selectedColumns;
  }
}
