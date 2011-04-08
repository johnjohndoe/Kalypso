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
package org.kalypso.model.wspm.tuhh.ui.panel;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author kimwerner
 */
public class WeirLabelProvider extends LabelProvider
{
  @Override
  public final String getText( final Object type )
  {
    if( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.17" ); //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.21" ); //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.24" ); //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_BEIWERT.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.27" ); //$NON-NLS-1$
    else
      return super.getText( type );
  }

  // FIXME: not used
  public final String getDescription( final String type )
  {
    if( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.18" ); //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.22" ); //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.25" ); //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_BEIWERT.equals( type ) )
      return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.28" ); //$NON-NLS-1$
    else
      return type;
  }

  public final String[] getTypes( )
  {
    return new String[] { IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG, IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG, IWspmTuhhConstants.WEHR_TYP_BREITKRONIG, IWspmTuhhConstants.WEHR_TYP_BEIWERT };
  }
}
