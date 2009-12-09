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

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */

public class DelRoughnessResolution extends AbstractProfilMarkerResolution
{

  private boolean m_initialized = false;

  private String m_roughness;

  private String[] m_roughnessTyps;

  public DelRoughnessResolution( )
  {
    this( new String[] { "" }, null ); //$NON-NLS-1$
    m_initialized = false;
  }

  public DelRoughnessResolution( final String[] roughnessTyps, final String roughnessToDelete )
  {
    super( Messages.getString( "Rauheiten entfernen" ), null, null );
    m_roughnessTyps = roughnessTyps;
    m_roughness = roughnessToDelete;
    m_initialized = true;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
    final StringBuffer params = new StringBuffer( super.getSerializedParameter() );
    params.append( ";" + (m_roughness == null ? "-" : m_roughness ));
    for( final String roughness : m_roughnessTyps )
    {
      params.append( ";" + roughness );
    }
    return params.toString(); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getUIresult(org.eclipse.swt.widgets.Shell)
   */
  @Override
  public String getUIresult( final Shell shell, final IProfil profil )
  {
    if( m_roughness != null )
      return m_roughness;

    final ListSelectionDialog lsd = new ListSelectionDialog( shell, m_roughnessTyps, new ArrayContentProvider()
    {
    }, new LabelProvider()
    {

      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        try
        {
          return profil.hasPointProperty( element.toString() ).getName();
        }
        catch( Exception e )
        {
          return element.toString();
        }
      }
    }, "Select the roughness to delete:" );

    if( lsd.open() == 0 )
    {
      final Object[] roughness = lsd.getResult();
      if( roughness.length > 0 && roughness[0] instanceof String )
      {
        m_roughness = (String) roughness[0];
      }
    }
    return m_roughness;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#hasUI()
   */
  @Override
  public boolean hasUI( )
  {
    return true;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  public boolean resolve( final IProfil profil )
  {
    if( m_initialized )
    {
      if( m_roughness == null )
      {
        try
        {
          m_roughness = getUIresult( PlatformUI.getWorkbench().getDisplay().getActiveShell(), profil );
        }
        catch( Exception e )
        {
          // handle exception: no shell available here
          return false;
        }
      }

      final IComponent comp = profil.hasPointProperty( m_roughness );
      if( comp != null )
      {
 //       final ProfilChangeHint hint =new ProfilChangeHint();
//       profil.fireProfilChanged( hint,new IProfilChange[]{new PointPropertyRemove( profil, comp ).doChange( hint )});
        
        profil.removePointProperty( comp );
//      final ProfilChangeHint hint =new ProfilChangeHint();
//      hint.setPointPropertiesChanged();
//      profil.fireProfilChanged( hint,new IProfilChange[]{});
        
        return true;
      }
        return false;

      


      
//      final ProfilOperation operation = new ProfilOperation( "Rauheit entfernen", profil, true );
//      operation.addChange( new PointPropertyRemove( profil, comp ) );
//      new ProfilOperationJob( operation ).schedule();


    }
    throw new IllegalStateException();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#setData(java.lang.String)
   */
  @Override
  public void setData( String parameterStream )
  {

    final String[] params = getParameter( parameterStream );
    try
    {
      m_roughness = "-".equals(  params[1])?null:params[1];
      final String[] rt = new String[params.length - 2];
      for( int i = 2; i < params.length; i++ )
      {
        rt[i - 2] = params[i];
      }
      m_roughnessTyps = rt;
      m_initialized = true;
    }
    catch( Exception e )
    {
      throw new IllegalArgumentException();
    }

  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#setUIresult(java.lang.String)
   */
  @Override
  public void setUIresult( String result )
  {
    m_roughness = result;
  }

}
