/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

import org.kalypso.ui.editor.styleeditor.MessageBundle;

public class LikeComparisonData extends AbstractComparisonData
{
  private String literal = null;

  private char escapeChar = '\\';

  private char singleChar = '?';

  private char wildCard = '*';

  public String getLiteral()
  {
    return literal;
  }

  public void setLiteral( String m_literal )
  {
    this.literal = m_literal.trim();
  }

  public boolean verify() throws FilterDialogException
  {
    if( literal == null || literal.trim().length() == 0 || propertyName == null )
    {
      throw new FilterDialogException( new FilterDialogError( null,
          MessageBundle.STYLE_EDITOR_FILTER_ERROR_INCOMPLETE ) );
    }

    return true;
  }

  public char getEscapeChar()
  {
    return escapeChar;
  }

  public void setEscapeChar( char m_escapeChar )
  {
    this.escapeChar = m_escapeChar;
  }

  public char getSingleChar()
  {
    return singleChar;
  }

  public void setSingleChar( char m_singleChar )
  {
    this.singleChar = m_singleChar;
  }

  public char getWildCard()
  {
    return wildCard;
  }

  public void setWildCard( char m_wildCard )
  {
    this.wildCard = m_wildCard;
  }
}