package org.kalypso.project.database.server.model;

import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;

public class HibernateUtil
{
  private static SessionFactory FACTORY;

  static
  {
    FACTORY = new AnnotationConfiguration().configure().buildSessionFactory();
  }

  public static SessionFactory getSessionFactory( )
  {
    return FACTORY;
  }

}
