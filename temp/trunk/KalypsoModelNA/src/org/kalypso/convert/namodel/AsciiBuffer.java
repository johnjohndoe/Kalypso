package org.kalypso.convert.namodel;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;

/**
 * @author doemming
 */
public class AsciiBuffer
{
  private final StringBuffer m_netBuffer = new StringBuffer();

  private final StringBuffer m_catchmentBuffer = new StringBuffer();

  private final StringBuffer m_channelBuffer = new StringBuffer();

  private final List m_featuresToWrite=new ArrayList();
  
  public AsciiBuffer()
  {
    // nothing to do here
  }

  public void addFeatureToWrite(Feature feature)
  {
    if(!m_featuresToWrite.contains(feature))
      m_featuresToWrite.add(feature);
  }
  
  public boolean writeFeature(Feature feature)
  {
    return m_featuresToWrite.contains(feature);
  }
  
  public StringBuffer getNetBuffer()
  {
    return m_netBuffer;
  }

  public StringBuffer getChannelBuffer()
  {
    return m_channelBuffer;
  }

  public StringBuffer getCatchmentBuffer()
  {
    return m_catchmentBuffer;
  }
}