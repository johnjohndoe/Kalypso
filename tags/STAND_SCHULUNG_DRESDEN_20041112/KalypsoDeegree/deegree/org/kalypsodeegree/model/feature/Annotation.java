package org.deegree.model.feature;

/**
 * @author belger
 */
public class Annotation
{

  private final String m_label;

  private final String m_tooltip;

  private final String m_description;

  private final String m_lang;

  public Annotation( String lang,String label,String tooltip,String description )
  {
    m_lang=lang;
    m_label = label;
    m_tooltip=tooltip;
    m_description=description;    
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
}