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
package org.kalypso.model.wspm.tuhh.ui.panel.buildings;

import java.util.ArrayList;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.IBeanValueProperty;
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
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;

/**
 * @author kimwerner
 */
public class BridgePanel extends AbstractProfilView
{
  protected ArrayList<PropertyLine> m_lines = new ArrayList<>( 8 );

  private FormToolkit m_toolkit = null;

  protected Composite m_propPanel;

  public BridgePanel( final IProfile profile )
  {
    super( profile );
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

      final String labelText = getLabel( m_property );
      m_label = toolkit.createLabel( parent, "" ); //$NON-NLS-1$
      m_label.setText( labelText );

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
            final BuildingBruecke building = WspmSohlpunkte.getBuilding( getProfile(), BuildingBruecke.class );
            if( building == null )
              return;

            final IBeanValueProperty beanValueProperty = BeanProperties.value( building.getClass(), m_property );
            final Double val = (Double)beanValueProperty.getValue( building );
            if( ObjectUtils.equals( val, value ) )
              return;

            beanValueProperty.setValue( building, new Double( value ) );
          }
        }
      } );
    }

    public void updateValue( )
    {
      if( m_text == null || m_text.isDisposed() || m_label == null || m_label.isDisposed() )
        return;

      final BuildingBruecke building = WspmSohlpunkte.getBuilding( getProfile(), BuildingBruecke.class );
      if( building == null )
        return;

      final IBeanValueProperty beanValueProperty = BeanProperties.value( building.getClass(), m_property );
      final Double val = (Double)beanValueProperty.getValue( building );
      final String textText = String.format( "%.3f", val ); //$NON-NLS-1$

      m_text.setText( textText );
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
    final BuildingBruecke building = WspmSohlpunkte.getBuilding( getProfile(), BuildingBruecke.class );
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

    final BuildingBruecke building = WspmSohlpunkte.getBuilding( getProfile(), BuildingBruecke.class );
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
    if( hint.isObjectChanged() || hint.isObjectDataChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            // createPropertyPanel();
            updateControls();
          }
        } );
      }
    }
  }
}