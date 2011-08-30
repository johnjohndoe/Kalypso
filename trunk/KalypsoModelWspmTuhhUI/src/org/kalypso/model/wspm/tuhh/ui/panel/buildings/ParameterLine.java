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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectEdit;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

public class ParameterLine
{
  protected final IProfilPointMarker m_devider;

  protected final Composite m_composite;

  private final IProfil m_profile;

  Text m_valueText;

  ParameterLine( final FormToolkit toolkit, final Composite parent, final IProfilPointMarker devider, final boolean canDelete, final IProfil profile )
  {
    m_profile = profile;
    m_composite = toolkit.createComposite( parent );
    final GridLayout layout = new GridLayout( 3, false );
    layout.marginLeft = 75;
    m_composite.setLayout( layout );
    m_composite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    m_devider = devider;

    toolkit.createLabel( m_composite, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.9" ) ); //$NON-NLS-1$

    m_valueText = toolkit.createText( m_composite, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_valueText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_valueText.setEnabled( m_devider != null );

    final BuildingWehr weir = WspmProfileHelper.getBuilding( m_profile, BuildingWehr.class );
    final Double coefficientValue;
    if( devider.getComponent().getId().equals( IWspmTuhhConstants.MARKER_TYP_WEHR ) )
    {
      coefficientValue = (Double) devider.getIntepretedValue();
    }
    else
    {
      coefficientValue = (Double) weir.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT );
    }
    if( coefficientValue != null )
    {
      m_valueText.setText( String.format( "%.4f", coefficientValue ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    final Display display = parent.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    m_valueText.addModifyListener( doubleModifyListener );

    m_valueText.addFocusListener( new FocusListener()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        if( m_valueText != null && !m_valueText.isDisposed() )
        {
          m_valueText.selectAll();
        }
      }

      @Override
      public void focusLost( final FocusEvent e )
      {
        if( m_valueText == null || m_valueText.isDisposed() )
          return;

        changeValue( m_valueText.getText() );
      }
    } );

    final DeleteWeirMarkerAction deleteMarkerAction = new DeleteWeirMarkerAction( m_devider, canDelete, profile );
    ActionButton.createButton( toolkit, m_composite, deleteMarkerAction );
  }

  protected void changeValue( final String text )
  {
    final double value = NumberUtils.parseQuietDouble( text );
    final Double valueToSet = Double.isNaN( value ) ? null : value;

    final BuildingWehr weir = WspmProfileHelper.getBuilding( m_profile, BuildingWehr.class );
    if( weir == null )
      return;

    /* Update text */
    if( valueToSet == null )
    {
      m_valueText.setText( StringUtils.EMPTY );
    }
    else
    {
      m_valueText.setText( String.format( "%.4f", valueToSet ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.11" ), m_profile, true ); //$NON-NLS-1$
    if( m_devider.getComponent().getId().equals( IWspmTuhhConstants.MARKER_TYP_WEHR ) )
    {
      operation.addChange( new PointMarkerEdit( m_devider, valueToSet ) );
    }
    else
    {
      operation.addChange( new ProfileObjectEdit( weir, weir.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ), valueToSet ) );
    }
    new ProfilOperationJob( operation ).schedule();
  }
}