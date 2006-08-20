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
package org.kalypso.model.wspm.ui.editor.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Event;
import org.kalypso.model.wspm.core.strang.IStranginfoListener;
import org.kalypso.model.wspm.core.strang.StrangInfo;


/**
 * @author gernot
 * 
 */
public class ProfilNavigateAction extends Action implements
    IStranginfoListener
{
  public static enum DIRECTION {
    PREV {
      @Override
      public int calcNewIndex( final StrangInfo info )
      {
        return Math.max( 0, info.getIndex() - 1 );
      }

      @Override
      public boolean isEnabled( final StrangInfo source )
      {
        return source != null && source.getIndex() > 0;
      }
    },
    NEXT {
      @Override
      public int calcNewIndex( final StrangInfo info )
      {
        return Math.min( info.size() - 1, info.getIndex() + 1 );
      }

      @Override
      public boolean isEnabled( final StrangInfo source )
      {
        return source != null && source.getIndex() < source.size() - 1;
      }
    };

    public abstract int calcNewIndex( final StrangInfo info );

    public abstract boolean isEnabled( final StrangInfo source );
  }

  private StrangInfo m_info = null;
  private final DIRECTION m_dir;

  public ProfilNavigateAction( final DIRECTION dir, final String name,
      final String tooltiptext, final ImageDescriptor imagedesc, final ImageDescriptor disabledImagedesc )
  {
    super( name, imagedesc );
    m_dir = dir;

    setDisabledImageDescriptor( disabledImagedesc );
    setToolTipText( tooltiptext );
  }

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setInfo( final StrangInfo info )
  {
    if( m_info != null )
      m_info.removeStranginfoListener( this );

    m_info = info;
    
    if( m_info != null )
      m_info.addStranginfoListener( this );

    onIndexChanged( m_info );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    if( m_info != null )
      m_info.setIndex( m_dir.calcNewIndex( m_info ) );
  }

  /**
   * @see com.bce.strang.IStranginfoListener#onTryChangeIndex(org.kalypso.model.wspm.core.strang.StrangInfo)
   */
  public boolean onTryChangeIndex( StrangInfo source )
  {
    return true;
  }
  
  /**
   * @see com.bce.strang.IStranginfoListener#onIndexChanged(com.bce.profil.eclipse.app.StrangInfo)
   */
  public void onIndexChanged( final StrangInfo source )
  {
    final boolean enabled = m_dir.isEnabled( source );
    setEnabled( enabled );
  }
}
