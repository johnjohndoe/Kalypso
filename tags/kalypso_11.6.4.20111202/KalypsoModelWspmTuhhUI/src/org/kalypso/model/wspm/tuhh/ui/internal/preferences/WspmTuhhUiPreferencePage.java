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
package org.kalypso.model.wspm.tuhh.ui.internal.preferences;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.preferences.PreferenceConstants;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * This class represents a preference page that is contributed to the Preferences dialog. By subclassing
 * <samp>FieldEditorPreferencePage </samp>, we can use the field support built into JFace that allows us to create a
 * page that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the preference store that belongs to the main
 * plug-in class. That way, preferences can be accessed directly via the preference store.
 */
public class WspmTuhhUiPreferencePage extends PreferencePage implements IWorkbenchPreferencePage
{
  private final WspmTuhhUiPreferenceData m_data = new WspmTuhhUiPreferenceData();

  private DataBindingContext m_binding;

  public WspmTuhhUiPreferencePage( )
  {
    super( Messages.getString( "WspmTuhhUiPreferencePage_0" ) ); //$NON-NLS-1$

    setDescription( Messages.getString( "WspmTuhhUiPreferencePage_1" ) ); //$NON-NLS-1$
  }

  @Override
  public void init( final IWorkbench workbench )
  {
  }

  @Override
  protected Control createContents( final Composite parent )
  {
    // Must be here, else its overwritten
    //setTitle( "KalypsoWSPM" ); //$NON-NLS-1$

    m_binding = new DataBindingContext();

    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    createWaterlevelMarkerRestriction( panel );

    return panel;
  }

  private void createWaterlevelMarkerRestriction( final Composite parent )
  {
    final String tooltip = String.format( Messages.getString( "WspmTuhhUiPreferencePage_2" ) ); //$NON-NLS-1$

    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString( "WspmTuhhUiPreferencePage_3" ) ); //$NON-NLS-1$
    label.setToolTipText( tooltip );

    final ComboViewer viewer = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    viewer.getControl().setToolTipText( tooltip );

    viewer.setContentProvider( new ArrayContentProvider() );

    viewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        if( element == StringUtils.EMPTY )
          return Messages.getString( "WspmTuhhUiPreferencePage_4" ); //$NON-NLS-1$

        final IComponent markerComponent = ComponentUtilities.getFeatureComponent( (String) element );
        return ComponentUtilities.getComponentName( markerComponent );
      }
    } );

    final Collection<String> input = new ArrayList<String>();
    input.add( StringUtils.EMPTY );
    input.add( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    viewer.setInput( input );

    /* Binding */
    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, PreferenceConstants.WATERLEVEL_RESTRICTION_MARKER );
    m_binding.bindValue( target, model );
  }

  @Override
  public boolean performOk( )
  {
    m_data.apply();

    return true;
  }

  @Override
  protected void performDefaults( )
  {
    m_data.restoreDefaults();
  }
}