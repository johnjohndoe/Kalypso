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
package org.kalypso.util.command;

import java.util.logging.Logger;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.i18n.Messages;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Job to process / undo / redo a ICommand Wird sofort gestartet
 */
public final class CommandJob extends Job
{
  public final static TYPE POST = new TYPE( "POST" ); //$NON-NLS-1$

  public final static TYPE UNDO = new TYPE( "UNDO" ); //$NON-NLS-1$

  public final static TYPE REDO = new TYPE( "REDO" ); //$NON-NLS-1$

  private final ICommand myCommand;

  private final ICommandManager myCommandManager;

  private static final Logger LOGGER = Logger.getLogger( CommandJob.class.getName() );

  private final Runnable myRunnable;

  private final TYPE m_type;

  public CommandJob( final ICommand command, final ICommandManager commandManager, final ISchedulingRule rule, final Runnable runnable, final TYPE type )
  {
    super( Messages.getString("org.kalypso.util.command.CommandJob.3") + getCommandDescription( commandManager, command, type ) ); //$NON-NLS-1$

    Assert.isNotNull( commandManager );
    Assert.isNotNull( command );

    // Nein: dann bleiben die Jobs als 'Waiting' in der ProgressAnzeige
    // setUser( true );

    setPriority( Job.SHORT );
    setRule( rule );

    myCommand = command;
    myCommandManager = commandManager;
    myRunnable = runnable;
    m_type = type;

    schedule();
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    final String description = getCommandDescription( myCommandManager, myCommand, m_type );

    LOGGER.info( m_type.toString() + ": " + description ); //$NON-NLS-1$

    try
    {
      if( m_type == POST )
        myCommandManager.postCommand( myCommand );
      else if( m_type == UNDO )
        myCommandManager.undo();
      else if( m_type == REDO )
        myCommandManager.redo();

      if( myRunnable != null )
        myRunnable.run();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      LOGGER.warning( Messages.getString("org.kalypso.util.command.CommandJob.5") + m_type + ": " + description ); //$NON-NLS-1$ //$NON-NLS-2$

      return new Status( IStatus.ERROR, KalypsoGisPlugin.getDefault().getBundle().getSymbolicName(), 0, Messages.getString("org.kalypso.util.command.CommandJob.7") + m_type + ": " + description, e ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    if( description == null )
      System.out.print( false );

    LOGGER.info( Messages.getString("org.kalypso.util.command.CommandJob.9") + m_type + ": " + description ); //$NON-NLS-1$ //$NON-NLS-2$

    return Status.OK_STATUS;
  }

  private static String getCommandDescription( final ICommandManager cm, final ICommand c, final TYPE type )
  {
    if( type == POST )
      return c.getDescription();
    else if( type == UNDO )
      return cm.getUndoDescription();
    else if( type == REDO )
      return cm.getRedoDescription();

    return ""; //$NON-NLS-1$
  }

  private static final class TYPE
  {
    private final String m_name;

    public TYPE( final String name )
    {
      m_name = name;
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString( )
    {
      return m_name;
    }
  }
}