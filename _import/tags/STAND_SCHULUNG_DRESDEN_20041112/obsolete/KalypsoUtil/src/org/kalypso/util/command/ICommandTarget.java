package org.kalypso.util.command;

/**
 * Helper-Interface f�r Command -Zeugs
 * 
 * @author belger
 */
public interface ICommandTarget
{
  public void postCommand( final ICommand command, final Runnable runnable );
}
