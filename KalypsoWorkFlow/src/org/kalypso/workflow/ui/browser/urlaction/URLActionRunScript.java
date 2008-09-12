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

import java.io.InputStream;
import java.net.URL;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.CommandURLUtilities;
import org.kalypso.workflow.ui.browser.ICommandURL;
import org.kalypso.workflow.ui.browser.urlaction.script.Function;
import org.kalypso.workflow.ui.browser.urlaction.script.Script;
import org.kalypso.workflow.ui.browser.urlaction.script.ScriptFactory;

/**
 * example<br>
 * kalypso://runScript?type=text/kalypso&function=test&location=url
 * <p>
 * html content
 * 
 * <pre>
 *                                                                       &lt;script type=&quot;text/kalypso&quot;&gt;
 *                                                                           &lt;!--
 *                                                                           function test() {
 *                                                                            kalypso://showMessage?message=test;
 *                                                                            kalypso://showMessage?message=test;
 *                                                                            kalypso://showMessage?message=test;
 *                                                                            }
 *                                                                            //--&gt;
 *                                                                        &lt;/script&gt;
 * </pre> }
 * 
 * @author doemming
 */
public class URLActionRunScript extends AbstractURLAction
{

  private final static String PARAM_FUNCTION = "function";

  /**
   * optional location for script<br>
   * default is browserContext
   */
  private final static String PARAM_SCRIPT_LOCATION = "location";

  private final static String PARAM_SCRIPT_TYPE = "type";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String functionName = commandURL.getParameter( PARAM_FUNCTION );
    final String scriptLocationAsString = commandURL.getParameter( PARAM_SCRIPT_LOCATION );
    final String scriptType = commandURL.getParameter( PARAM_SCRIPT_TYPE );
    try
    {
      final URL url = getWorkFlowContext().resolveURL( scriptLocationAsString );
      final InputStream stream = url.openStream();
      final String content = IOUtils.toString( stream );
      final Script script = ScriptFactory.createScript( content, scriptType );
      final Function function = script.getFunction( functionName );
      if( function == null )
        throw new Exception( "Function (" + functionName + ") in skript defined on html-page\n " + url.toExternalForm() + "\n was not found!" );
      final List<String> commands = function.getCommands();
      return CommandURLUtilities.runCommands( getWorkFlowContext(), commands );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return generateMessageDialog( e.getMessage(), IStatus.ERROR );
    }
  }

}
