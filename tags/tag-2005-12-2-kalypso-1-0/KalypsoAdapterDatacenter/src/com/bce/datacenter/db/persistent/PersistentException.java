package com.bce.datacenter.db.persistent;

/**
 * 
 * Persistent Exception for persistent related exceptions
 * 
 * @author Marc Schlienger
 */
public class PersistentException extends Exception
{
  public PersistentException( )
  {
    super();
  }

  public PersistentException( String message )
  {
    super( message );
  }

  public PersistentException( Throwable cause )
  {
    super( cause );
  }

  public PersistentException( String message, Throwable cause )
  {
    super( message, cause );
  }

  public PersistentException( Persistent p )
  {
    this( "Object invalid exception" + p.getID() );
  }
}