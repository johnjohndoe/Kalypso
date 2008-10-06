package org.kalypsodeegree.model.table;

public class TableException extends Exception
{
  private String m_message = "org.kalypsodeegree.model.table.TableException: ";

  public TableException( final String message )
  {
    super( message );
    
    m_message = m_message + message;
  }

  @Override
  public String toString()
  {
    return m_message;
  }

}