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
package org.kalypso.workflow.ui.browser.urlaction;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import org.eclipse.jface.dialogs.MessageDialog;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * kalypso://showMessage?message=example&title=Warning
 * 
 * @author doemming
 */
public class URLActionShowMessage extends AbstractURLAction
{
  // private final static String COMMAND_NAME = "showMessage";

  /** optional */
  private final static String PARAM_TITLE = "title";

  private final static String PARAM_MESSAGE = "message";

  private final static String PARAM_DECODE = "decode";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String message = commandURL.getParameter( PARAM_MESSAGE );
    if( message == null || message.length() == 0 )
      return false;
    final String title = commandURL.getParameter( PARAM_TITLE );
    boolean decoding = Boolean.parseBoolean( commandURL.getParameter( PARAM_DECODE ) );
    final String decodedMessage;
    if( decoding )
    {
      try
      {
        decodedMessage = URLDecoder.decode( message, "UTF-8" );
      }
      catch( UnsupportedEncodingException e )
      {
        e.printStackTrace();
        return false;
      }
    }
    else
      decodedMessage = message;
    MessageDialog.openInformation( getShell(), title, decodedMessage );
    return true;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#getActionName()
   */
  public String getActionName( )
  {
    return m_commandName;
  }
}
