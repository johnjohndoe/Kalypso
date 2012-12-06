package org.kalypso.ewawi.shape.writer;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author Holger Albert
 */
public class FotoListData
{
  private final String m_filename;

  private final BigDecimal m_rechtswert;

  private final BigDecimal m_hochwert;

  private final BigDecimal m_hoehe;

  private final Date m_datum;

  public FotoListData( final String filename, final BigDecimal rechtswert, final BigDecimal hochwert, final BigDecimal hoehe, final Date datum )
  {
    m_filename = filename;
    m_rechtswert = rechtswert;
    m_hochwert = hochwert;
    m_hoehe = hoehe;
    m_datum = datum;
  }

  public String getFilename( )
  {
    return m_filename;
  }

  public BigDecimal getRechtswert( )
  {
    return m_rechtswert;
  }

  public BigDecimal getHochwert( )
  {
    return m_hochwert;
  }

  public BigDecimal getHoehe( )
  {
    return m_hoehe;
  }

  public Date getDatum( )
  {
    return m_datum;
  }
}