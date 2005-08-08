/**
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestraße 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.contribs.eclipse.core.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;

/**
 * Helper methods for {@link org.eclipse.core.runtime.IStatus}.
 * 
 * @author thuel2
 */
public final class StatusUtilities
{
  private StatusUtilities()
  {
  // wird nicht instantiiert
  }

  /**
   * generates MultiStatus to make sure that each line of the message can be seen as a single line in an errorDialog.
   * Severety, pluginId and code remain the same for each Status in the MultiStatus.
   * 
   * @param throwable
   */
  public static MultiStatus createMultiStatusFromMessage( final int severity, final String pluginId, final int code,
      final String message, final String delim, final Throwable throwable )
  {
    StringTokenizer strTok;
    String sMainMessage;
    strTok = new StringTokenizer( message, delim );
    final Collection stati = new ArrayList( strTok.countTokens() - 1 > 0 ? strTok.countTokens() - 1 : 0 );
    if( strTok.hasMoreTokens() )
    {
      sMainMessage = strTok.nextToken();
      while( strTok.hasMoreTokens() )
      {
        stati.add( new Status( severity, pluginId, code, strTok.nextToken(), null ) );
      }
    }
    else
    {
      sMainMessage = message;
    }
    // Child ohne Message generieren, damit der MultiStatus auf jeden Fall über
    // die severity informirt ist
    stati.add( new Status( severity, pluginId, code, "", null ) );

    final IStatus[] childStati = (IStatus[])stati.toArray( new IStatus[stati.size()] );
    return new MultiStatus( pluginId, code, childStati, sMainMessage, throwable );
  }

  /**
   * Returns the message of the given status.
   * <p>
   * If the status is a multi-status, it recursively creates a string with all includes child-stati, separated by
   * line-breaks.
   * </p>
   */
  public static String messageFromStatus( final IStatus status )
  {
    return createStringFromStatus( status, 0 );
  }

  /**
   * Returns the message form the given status. If the status is a multi-status, it recursively creates a string with
   * all includes child-stati, separated by line-breaks
   * 
   * @param currentDepth
   *          Amout of tabs with wich the message will be indentated
   */
  private static String createStringFromStatus( final IStatus status, final int currentDepth )
  {
    final StringBuffer tabBuffer = new StringBuffer();
    for( int i = 0; i < currentDepth; i++ )
      tabBuffer.append( '\t' );

    if( !status.isMultiStatus() )
    {
      final StringBuffer statusBuffer = new StringBuffer( tabBuffer.toString() );
      statusBuffer.append( status.getMessage() );
      final Throwable exception = status.getException();
      if( exception != null )
      {
        final String localizedMessage = exception.getLocalizedMessage();
        if( localizedMessage != null )
        {
          statusBuffer.append( "\n\t" );
          statusBuffer.append( tabBuffer.toString() );
          statusBuffer.append( localizedMessage );
        }
      }

      return statusBuffer.toString();
    }

    final StringBuffer buffer = new StringBuffer( tabBuffer.toString() );
    buffer.append( status.getMessage() );
    buffer.append( '\n' );

    final IStatus[] children = status.getChildren();
    for( int i = 0; i < children.length; i++ )
    {
      buffer.append( createStringFromStatus( children[i], currentDepth + 1 ) );
      buffer.append( '\n' );
    }

    return buffer.toString();
  }
}