package org.deegree_impl.model.feature.xlink;

/**
 * @author sbad0205
 */
public class XLinkArc
{
  private final String myLabelFrom;
  private final String myLabelTo;
  
  public XLinkArc(String labelFrom,String labelTo)
  {
    myLabelFrom=labelFrom;
    myLabelTo=labelTo;
  }
  
  public String getFromLabel()
  {
    return myLabelFrom;
    
  }
  
  public String getLabelTo()
  {
      return myLabelTo;
  }
}
