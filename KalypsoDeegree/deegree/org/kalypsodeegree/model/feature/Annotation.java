package org.kalypsodeegree.model.feature;

/**
 * @author belger
 */
public class Annotation
{

  private final String m_label;

  private final String m_tooltip;

  private final String m_description;

  private final String m_lang;

  public static final String UNSUPPORTED_LANG_KEY = "unsupportedLanuage";

  public static final String NO_LANG_KEY = "nolangkey";

  public Annotation( String lang, String label, String tooltip, String description )
  {
    m_lang = lang;
    m_label = label;
    m_tooltip = tooltip;
    m_description = description;
  }

  public String getDescription()
  {
    return m_description;
  }

  public String getLabel()
  {
    return m_label;
  }

  public String getTooltip()
  {
    return m_tooltip;
  }

  public String getLang()
  {
    return m_lang;
  }

  public String toString()
  {
    final StringBuffer b = new StringBuffer( "[" + m_lang + "]" );
    b.append( "\n label:" );
    b.append( m_label );
    b.append( "\n tooltip:" );
    b.append( m_tooltip );
    b.append( "\n description:" );
    b.append( m_description );
    return b.toString();
  }
}