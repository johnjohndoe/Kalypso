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
/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

import org.kalypso.ui.editor.styleeditor.MessageBundle;

public class BetweenComparisonData extends AbstractComparisonData
{
  private String lower = null;

  private String upper = null;

  public String getLower()
  {
    return lower;
  }

  public void setLower( String m_lower )
  {
    this.lower = m_lower.trim();
  }

  public String getUpper()
  {
    return upper;
  }

  public void setUpper( String m_upper )
  {
    this.upper = m_upper.trim();
  }

  public boolean verify() throws FilterDialogException
  {
    if( lower == null || lower.trim().length() == 0 || upper == null || upper.trim().length() == 0
        || propertyName == null )
    {
      throw new FilterDialogException( new FilterDialogError( null, MessageBundle.STYLE_EDITOR_FILTER_ERROR_INCOMPLETE ) );
    }
    try
    {
      double lowerDouble = Double.parseDouble( lower );
      double upperDouble = Double.parseDouble( upper );
      if( lowerDouble > upperDouble )
        throw new FilterDialogException( new FilterDialogError( null,
            MessageBundle.STYLE_EDITOR_FILTER_ERROR_LOWERBOUNDARY_EXCEEDS_UPPERBOUNDARY ) );
    }
    catch( NumberFormatException e )
    {
      throw new FilterDialogException( new FilterDialogError( null, MessageBundle.STYLE_EDITOR_FILTER_ERROR_LOWER_UPPER
          + MessageBundle.STYLE_EDITOR_ERROR_NUMBER ) );
    }
    return true;

  }
}