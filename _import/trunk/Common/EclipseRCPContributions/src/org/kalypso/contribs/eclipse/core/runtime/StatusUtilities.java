/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/
package org.kalypso.contribs.eclipse.core.runtime;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.EclipseRCPContributionsPlugin;

/**
 * Helper methods for {@link org.eclipse.core.runtime.IStatus}.
 * 
 * @author thuel
 */
public final class StatusUtilities
{
  /**
   * A status mask representing the combination of all available stati. Usefull e.g. for
   * {@link org.eclipse.jface.dialogs.ErrorDialog#openError(org.eclipse.swt.widgets.Shell, java.lang.String, java.lang.String, org.eclipse.core.runtime.IStatus, int)}.
   */
  public static final int ALL_STATUS_MASK = IStatus.OK | IStatus.CANCEL | IStatus.INFO | IStatus.WARNING
      | IStatus.ERROR;

  private StatusUtilities()
  {
  // wird nicht instantiiert
  }

  /**
   * Generates MultiStatus to make sure that each line of the message can be seen as a single line in an errorDialog.
   * Severety, pluginId and code remain the same for each Status in the MultiStatus.
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

  /**
   * Transforms any exception into an {@link IStatus}object.
   * <p>
   * If the exception is an {@link InvocationTargetException}the inner exception is wrapped instead.
   * </p>
   * <p>
   * If the exception is a {@link CoreException}its status is returned.
   * </p>
   * 
   * @throws NullPointerException
   *           If <code>t</code> is null.
   */
  public static IStatus statusFromThrowable( final Throwable t )
  {
    return statusFromThrowable( t, null );
  }

  /**
   * Transforms any exception into an {@link IStatus}object.
   * <p>
   * If the exception is an {@link InvocationTargetException}the inner exception is wrapped instead.
   * </p>
   * <p>
   * If the exception is a {@link CoreException}its status is returned.
   * </p>
   * 
   * @param message
   *          [optional] used as message for newly created status if specified
   * 
   * @throws NullPointerException
   *           If <code>t</code> is null.
   */
  public static IStatus statusFromThrowable( final Throwable t, final String message )
  {
    if( t instanceof InvocationTargetException )
      return statusFromThrowable( ( (InvocationTargetException)t ).getCause(), message );
    if( t instanceof CoreException )
      return ( (CoreException)t ).getStatus();

    String msg = t.getLocalizedMessage();
    if( msg == null )
      msg = "<Keine weitere Information vorhanden>";

    if( message != null )
      msg = message + ". " + msg;

    return new Status( IStatus.ERROR, EclipseRCPContributionsPlugin.getID(), 0, msg, t );
  }

  /**
   * Creates a status based on the list of stati. If the list is empty, it returns the <code>Status.OK_STATUS</code>.
   * If the list contains just one status, then it is returned. If the list contains more than one status, a MultiStatus
   * is returned.
   * 
   * @param message
   *          only used when creating the MultiStatus
   */
  public static IStatus createStatus( final List stati, final String message )
  {
    if( stati.size() == 0 )
      return Status.OK_STATUS;

    if( stati.size() == 1 )
      return (IStatus)stati.get( 0 );

    return new MultiStatus( EclipseRCPContributionsPlugin.getID(), 0, (IStatus[])stati.toArray( new IStatus[stati
        .size()] ), message, null );
  }

  /**
   * Creates a status based on the list of stati. If the list is empty, it returns the <code>Status.OK_STATUS</code>.
   * If the list contains just one status, then it is returned. If the list contains more than one status, a MultiStatus
   * is returned.
   * 
   * @param message
   *          only used when creating the MultiStatus
   */
  public static IStatus createStatus( final IStatus[] stati, final String message )
  {
    return createStatus( Arrays.asList( stati ), message );
  }

  /**
   * Creates a status with given severity, message, and throwable
   */
  public static IStatus createStatus( final int severity, final String message, final Throwable t )
  {
    return new Status( severity, EclipseRCPContributionsPlugin.getID(), 0, message, t );
  }

  /**
   * Creates an error-status with given message and null throwable.
   */
  public static IStatus createErrorStatus( final String errorMessage )
  {
    return createStatus( IStatus.ERROR, errorMessage, null );
  }

  /**
   * Creates an info-status with given message and null throwable.
   */
  public static IStatus createInfoStatus( final String infoMessage )
  {
    return createStatus( IStatus.INFO, infoMessage, null );
  }

  /**
   * Creates a warning-status with given message and null throwable.
   */
  public static IStatus createWarningStatus( final String warningMessage )
  {
    return createStatus( IStatus.WARNING, warningMessage, null );
  }

  /**
   * Wraps the given status in a new status with the given severity. If the given status has already the given severity,
   * then it is simply returned.
   * 
   * @param status
   *          the status to wrap
   * @param severity
   *          the desired severity
   * @param severityMask
   *          the severity-mask for which the wrapping takes place. If the given status does not match this
   *          severity-mask, no wrap takes place
   */
  public static IStatus wrapStatus( final IStatus status, final int severity, final int severityMask )
  {
    if( status.matches( severity ) || !status.matches( severityMask ) )
      return status;

    final IStatus newStatus;
    if( status.isMultiStatus() )
    {
      newStatus = new MultiStatus( status.getPlugin(), status.getCode(), ( (MultiStatus)status ).getChildren(), status
          .getMessage(), status.getException() )
      {
        public int getSeverity()
        {
          return severity;
        }
      };
    }
    else
    {
      newStatus = new Status( severity, status.getPlugin(), status.getCode(), status.getMessage(), status
          .getException() )
      {
        public int getSeverity()
        {
          return severity;
        }
      };
    }

    return newStatus;
  }
}