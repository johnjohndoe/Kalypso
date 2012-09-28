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
package org.kalypso.model.wspm.tuhh.ui.panel.buildings;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.IBeanValueProperty;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;

/**
 * @author kimwerner
 */
public class CulvertPanel extends AbstractProfilView
{
  protected ArrayList<PropertyLine> m_lines = new ArrayList<>( 8 );

  private FormToolkit m_toolkit = null;

  protected Composite m_propPanel;

  private ComboViewer m_cmb = null;

  private final Map<String, IProfileBuilding> m_culverts = new HashMap<>();

  public CulvertPanel( final IProfile profile )
  {
    super( profile );

    m_culverts.put( BuildingKreis.ID, new BuildingKreis() );
    m_culverts.put( BuildingTrapez.ID, new BuildingTrapez() );
    m_culverts.put( BuildingMaul.ID, new BuildingMaul() );
    m_culverts.put( BuildingEi.ID, new BuildingEi() );
  }

  private class PropertyLine
  {
    protected final String m_property;

    protected final Text m_text;

    protected final Label m_label;

    public PropertyLine( final FormToolkit toolkit, final Composite parent, final String property )
    {
      m_property = property;

      final Display display = parent.getDisplay();
      final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
      final Color badColor = display.getSystemColor( SWT.COLOR_RED );
      final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

      m_label = toolkit.createLabel( parent, getLabel( m_property ) );

      m_text = toolkit.createText( parent, null, SWT.FILL | SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
      m_text.setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false ) );
      m_text.addModifyListener( doubleModifyListener );
      m_text.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( final FocusEvent e )
        {
          if( m_text != null && !m_text.isDisposed() )
            m_text.selectAll();
        }

        @Override
        public void focusLost( final FocusEvent e )
        {
          final double value = NumberUtils.parseQuietDouble( m_text.getText() );
          if( !Double.isNaN( value ) )
          {
            final ICulvertBuilding building = WspmSohlpunkte.getBuilding( getProfile(), ICulvertBuilding.class );
            if( building == null )
              return;

            final IBeanValueProperty beanValueProperty = BeanProperties.value( building.getClass(), m_property );
            final Double val = (Double)beanValueProperty.getValue( building );
            if( ObjectUtils.equals( value, val ) )
              return;

            beanValueProperty.setValue( building, new Double( value ) );
          }
        }
      } );
    }

    public void updateValue( )
    {
      if( m_label == null || m_label.isDisposed() )
        return;

      m_label.setText( getLabel( m_property ) );
      if( m_text == null || m_text.isDisposed() )
        return;

      final ICulvertBuilding building = WspmSohlpunkte.getBuilding( getProfile(), ICulvertBuilding.class );
      if( building == null )
        return;

      final IBeanValueProperty beanValueProperty = BeanProperties.value( building.getClass(), m_property );
      final Double val = (Double)beanValueProperty.getValue( building );
      m_text.setText( String.format( "%.4f", val ) ); //$NON-NLS-1$
      if( m_text.isFocusControl() )
        m_text.selectAll();
    }

    public void dispose( )
    {
      m_text.dispose();
      m_label.dispose();
    }
  }

  protected String getLabel( final String property )
  {
    final ICulvertBuilding building = WspmSohlpunkte.getBuilding( getProfile(), ICulvertBuilding.class );
    if( building != null )
      return building.getPropertyLabel( property );

    return property;
  }

  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    m_toolkit = toolkit;
    m_propPanel = m_toolkit.createComposite( parent );
    m_propPanel.setLayout( new GridLayout( 2, false ) );

    m_toolkit.createLabel( m_propPanel, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.TubePanel.7" ), SWT.NONE ); //$NON-NLS-1$

    m_cmb = new ComboViewer( m_propPanel );
    m_cmb.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.CENTER, false, false ) );
    m_cmb.setContentProvider( new ArrayContentProvider() );
    m_cmb.setInput( m_culverts.values() );
    m_cmb.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        return ((IProfileObject)element).getTypeLabel();
      }
    } );

    m_cmb.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)event.getSelection();

        final ICulvertBuilding tube = (ICulvertBuilding)selection.getFirstElement();

        final ICulvertBuilding old = WspmSohlpunkte.getBuilding( getProfile(), ICulvertBuilding.class );
        if( tube != null && !tube.getId().equals( old.getId() ) )
        {
          // TODO use copy constructor with IDurchlass Interfac
          // tube.cloneValuesFrom( old );
          getProfile().addProfileObjects( new IProfileObject[] { tube } );
        }
      }
    } );
    m_toolkit.adapt( m_cmb.getCombo() );

    final ICulvertBuilding building = WspmSohlpunkte.getBuilding( getProfile(), ICulvertBuilding.class );
    if( building != null )
      m_cmb.setSelection( new StructuredSelection( m_culverts.get( building.getId() ) ) );

    final Label spacer = m_toolkit.createSeparator( m_propPanel, SWT.SEPARATOR | SWT.HORIZONTAL );
    spacer.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 ) );
    createPropertyPanel();
    updateControls();

    return m_propPanel;
  }

  protected void createPropertyPanel( )
  {
    for( final PropertyLine line : m_lines )
    {
      line.dispose();
    }

    m_lines = new ArrayList<>( 8 );

    final ICulvertBuilding building = WspmSohlpunkte.getBuilding( getProfile(), ICulvertBuilding.class );
    if( building == null )
      return;

    for( final String property : building.getProperties() )
      m_lines.add( new PropertyLine( m_toolkit, m_propPanel, property ) );

    m_propPanel.layout();
  }

  protected void updateControls( )
  {
    for( final PropertyLine line : m_lines )
      line.updateValue();
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isObjectDataChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            updateControls();
          }
        } );
      }
    }
  }
}