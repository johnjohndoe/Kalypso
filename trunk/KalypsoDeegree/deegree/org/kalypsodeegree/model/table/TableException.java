package org.kalypsodeegree.model.table;

public class TableException extends Exception
{

  private String message = "org.kalypsodeegree.model.table.TableException: ";

  public TableException( String message )
  {
    super( message );
    this.message = this.message + message;
  }

  public String toString()
  {
    return message;
  }

}