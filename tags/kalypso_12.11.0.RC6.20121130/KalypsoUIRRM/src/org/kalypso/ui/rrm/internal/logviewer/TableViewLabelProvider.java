/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.logviewer;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * Label provider for table cell columns
 * 
 * @author Madan,huebsch
 */
public class TableViewLabelProvider extends LabelProvider implements ITableLabelProvider
{
  @Override
  public String getColumnText( final Object obj, final int index )
  {
    if( obj instanceof LogFileRow )
    {
      switch( index )
      {
        case 0:
          return ((LogFileRow) obj).getLevel();
        case 1:
          return ((LogFileRow) obj).getElement();
        case 2:
          return ((LogFileRow) obj).getMessage();
        case 3:
          return ((LogFileRow) obj).getParameter();

      }
    }
    return ""; //$NON-NLS-1$
  }

  @Override
  public Image getColumnImage( final Object obj, final int index )
  {
    if( obj instanceof LogFileRow )
    {
      switch( index )
      {
        case 0:
        {
          final String logLevel = (((LogFileRow) obj).getLevel()).trim();
          if( (logLevel).equals( "SEVERE" ) ) //$NON-NLS-1$
            return PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_OBJS_ERROR_TSK );
          else if( logLevel.equals( "WARNING" ) ) //$NON-NLS-1$
            return PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_OBJS_WARN_TSK );
          else if( logLevel.equals( "INFO" ) || logLevel.equals( "CONFIG" ) || logLevel.equals( "FINE" ) || logLevel.equals( "FINER" ) || logLevel.equals( "FINEST" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            return PlatformUI.getWorkbench().getSharedImages().getImage( ISharedImages.IMG_OBJS_INFO_TSK );
        }
      }
    }
    return null;
  }
}