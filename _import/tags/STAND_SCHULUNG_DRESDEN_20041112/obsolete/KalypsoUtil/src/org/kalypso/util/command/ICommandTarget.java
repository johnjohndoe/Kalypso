package org.kalypso.util.command;

/**
 * Helper-Interface für Command -Zeugs
 * 
 * @author belger
 */
public interface ICommandTarget
{
  public void postCommand( final ICommand command, final Runnable runnable );
}
