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
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IRecord;

class DeviderLine
{
  private final int m_deviderID;

  private final String m_componentID;

  final Text m_position;

  private final IProfile m_profile;

  public DeviderLine( final FormToolkit toolkit, final Composite parent, final int deviderID, final String componentID, final boolean canAdd, final IProfile profile )
  {
    m_deviderID = deviderID;
    m_componentID = componentID;
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
    m_position.setEnabled( m_deviderID > -1 );
    final AddWeirDeviderAction addDeviderAction = new AddWeirDeviderAction( profile, m_deviderID, m_componentID, canAdd );
    final Button btnAdd = ActionButton.createButton( toolkit, composite, addDeviderAction );
    btnAdd.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

    m_position.addModifyListener( doubleModifyListener );
    m_position.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        if( m_position != null && !m_position.isDisposed() )
        {
          m_position.selectAll();
        }
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

  private IProfilePointMarker getDevider( )
  {
    final IProfilePointMarker[] pointMarker = m_profile.getPointMarkerFor( m_componentID );
    if( pointMarker == null || pointMarker.length <= m_deviderID )
      return null;

    return pointMarker[m_deviderID];
  }

  protected void addDevider( final String text )
  {
    if( getDevider() == null )
      return;

    final double value = NumberUtils.parseQuietDouble( text );
    if( Double.isNaN( value ) )
      return;

    final Double pos = ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, getDevider().getPoint() );
    if( ProfileUtil.compareValues( value, pos, 0.0001 ) )
      return;

    final IProfileRecord point = ProfileVisitors.findNearestPoint( m_profile, value );

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.2" ), m_profile, true ); //$NON-NLS-1$
    operation.addChange( new PointMarkerSetPoint( getDevider(), point ) );
    operation.addChange( new ActiveObjectEdit( m_profile, point.getBreiteAsRange(), null ) );
    new ProfileOperationJob( operation ).schedule();
  }

  public void refresh( )
  {
    if( getDevider() == null )
    {
      m_position.setText( StringUtils.EMPTY );
    }
    else
    {
      final IRecord point = getDevider().getPoint();
      m_position.setText( String.format( "%.4f", ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point ) ) ); //$NON-NLS-1$
    }
  }
}