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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import java.util.Collections;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class AddRoughnessResolution extends AbstractProfilMarkerResolution
{
  private boolean m_initialized = false;

  private String[] m_roughnessTyps;

  private String m_roughnessToAdd = null;

  public AddRoughnessResolution( )
  {
    this( new String[] {} ); //$NON-NLS-1$

    m_initialized = false;
  }

  public AddRoughnessResolution( final String[] roughnessTyps )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.resolutions.AddRoughnessResolution.0" ), null, null ); //$NON-NLS-1$

    m_roughnessTyps = roughnessTyps;
    m_initialized = true;
  }

  @Override
  public String getSerializedParameter( )
  {
    final StringBuilder params = new StringBuilder( super.getSerializedParameter() );
    params.append( ';' );

    params.append( StringUtils.join( m_roughnessTyps, ';' ) );

    return params.toString();
  }

  @Override
  public String getUIresult( final Shell shell, final IProfile profil )
  {
    final LabelProvider labelProvider = new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        try
        {
          return profil.getPointPropertyFor( element.toString() ).getName();
        }
        catch( final Exception e )
        {
          return element.toString();
        }
      }
    };

    final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.resolutions.DelRoughnessResolution.4" ); //$NON-NLS-1$

    final ListDialog dialog = new ListDialog( shell );
    dialog.setAddCancelButton( true );
    dialog.setContentProvider( new ArrayContentProvider() );
    dialog.setLabelProvider( labelProvider );
    dialog.setMessage( msg );
    dialog.setInput( m_roughnessTyps );
    dialog.setInitialElementSelections( Collections.singletonList( m_roughnessTyps[0] ) );
    dialog.setTitle( getLabel() );

    if( dialog.open() == Window.OK )
    {
      final Object[] roughness = dialog.getResult();
      if( roughness.length > 0 && roughness[0] instanceof String )
        return (String) roughness[0];

      return null;
    }
    else
      return null;
  }

  @Override
  public boolean hasUI( )
  {
    return true;
  }

  @Override
  public boolean resolve( final IProfile profil )
  {
    if( m_initialized )
    {
      m_roughnessToAdd = getUIresult( PlatformUI.getWorkbench().getDisplay().getActiveShell(), profil );
      /* Return on cancel */
      if( m_roughnessToAdd == null )
        return false;

      final IComponent comp = profil.hasPointProperty( m_roughnessToAdd );
      if( comp == null )
        profil.addPointProperty( profil.getPointPropertyFor( m_roughnessToAdd ), 0.0 );

      return true;
    }
    throw new IllegalStateException();
  }

  @Override
  public void setData( final String parameterStream )
  {
    final String[] params = getParameter( parameterStream );
    try
    {
      final String[] rt = new String[params.length - 1];
      for( int i = 1; i < params.length; i++ )
        rt[i - 1] = params[i];

      m_roughnessTyps = rt;
      m_initialized = true;
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }
  }

  @Override
  public void setUIresult( final String result )
  {
    m_roughnessToAdd = result;
  }
}
