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
package org.kalypso.model.wspm.tuhh.ui.panel;

import org.apache.commons.lang.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.observation.result.IRecord;

class DeviderLine
{
  protected final IProfilPointMarker m_devider;

  final Text m_position;

  private final IProfil m_profile;

  public DeviderLine( final FormToolkit toolkit, final Composite parent, final IProfilPointMarker devider, final boolean canAdd, final IProfil profile )
  {
    m_devider = devider;
    m_profile = profile;

    final Composite composite = toolkit.createComposite( parent );
    composite.setLayout( new GridLayout( 3, false ) );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false, 2, 1 ) );

    final Display display = parent.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    toolkit.createLabel( composite, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.0" ) ); //$NON-NLS-1$

    m_position = toolkit.createText( composite, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_position.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_position.setEnabled( m_devider != null );

// final Label spacer = m_toolkit.createSeparator( m_composite, SWT.SEPARATOR | SWT.HORIZONTAL );
// spacer.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    final AddWeirDeviderAction addDeviderAction = new AddWeirDeviderAction( profile, m_devider, canAdd );
    final Button btnAdd = ActionButton.createButton( toolkit, composite, addDeviderAction );
    btnAdd.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

    m_position.addModifyListener( doubleModifyListener );
    m_position.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        if( m_position != null && !m_position.isDisposed() )
          m_position.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        if( m_position == null || m_position.isDisposed() )
          return;

        addDevider( m_position.getText() );
      }
    } );
  }

  protected void addDevider( final String text )
  {
    if( m_devider == null )
      return;

    final double value = NumberUtils.parseQuietDouble( text );
    if( Double.isNaN( value ) )
      return;

    final Double pos = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, m_devider.getPoint() );
    if( ProfilUtil.compareValues( value, pos, 0.0001 ) )
      return;

    final IRecord point = ProfilUtil.findNearestPoint( m_profile, value );
    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.2" ), m_profile, true ); //$NON-NLS-1$
    operation.addChange( new PointMarkerSetPoint( m_devider, point ) );
    operation.addChange( new ActiveObjectEdit( m_profile, point, null ) );
    new ProfilOperationJob( operation ).schedule();
  }

  public void refresh( )
  {
    if( m_devider == null )
      m_position.setText( StringUtils.EMPTY );
    else
    {
      final IRecord point = m_devider.getPoint();
      m_position.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point ) ) ); //$NON-NLS-1$
    }
  }
}