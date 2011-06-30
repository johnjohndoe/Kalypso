package org.kalypso.model.wspm.pdb.db.mapping;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * Info generated by hbm2java
 */
@Entity
@Table(name = "info", schema = "pdb_admin")
public class Info implements java.io.Serializable
{

  private String key;

  private String value;

  public Info( )
  {
  }

  public Info( final String key )
  {
    this.key = key;
  }

  public Info( final String key, final String value )
  {
    this.key = key;
    this.value = value;
  }

  @Id
  @Column(name = "key", unique = true, nullable = false, length = 50)
  public String getKey( )
  {
    return this.key;
  }

  public void setKey( final String key )
  {
    this.key = key;
  }

  @Column(name = "value")
  public String getValue( )
  {
    return this.value;
  }

  public void setValue( final String value )
  {
    this.value = value;
  }

}
