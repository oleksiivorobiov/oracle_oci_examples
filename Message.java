/* $Header: Message.java 15-mar-00.09:10:14 rbhyrava Exp $ */

/* Copyright (c) Oracle Corporation 2000. All Rights Reserved. */

/*
   DESCRIPTION
    <short description of component this file declares/defines>

   PRIVATE CLASSES
    <list of private classes defined - with one-line descriptions>

   NOTES
    <other useful comments, qualifications, etc.>

   MODIFIED    (MM/DD/YY)
    rbhyrava    03/15/00 - AQ jms demo -Message Object Type
    rbhyrava    03/15/00 - Creation
 */

/**
 *  @version $Header: Message.java 15-mar-00.09:10:14 rbhyrava Exp $
 *  @author  rbhyrava
 *  @since   release specific (what release of product did this appear in)
 */

/*
**  Definition of Serial Object for Object Message tests
**  simple object definition, extends serializable
**  Refer to aqjmsdemo01.java 
*/
import java.io.*;
import oracle.AQ.*;
import oracle.jms.*;
import javax.jms.*;
import java.lang.*;
import java.math.*;
import java.util.*;

class Message implements Serializable
{
  String name;
  int    id;
  int    num_entries;
  int[]  data_points;

  Message()
  {
    name = null;
    id = -99;
    num_entries = 0;
    data_points = null;
  }

  public String getName()
  {
    return name;
  }

  public int getId()
  {
    return id;
  }

  public int getNumEntries()
  {
    return num_entries;
  }

  public int[] getData()
  {
    return data_points;
  }

  public void setName(String nam)
  {
    name = nam;
  }

  public void setId(int ID)
  {
    id = ID;
  }
  
  public void setData(int n)
  {
    int i = 0;
    data_points = new int[n];
    for ( i=0; i < n; i++ )
    {
       data_points[i] = i;
    } 
    num_entries = data_points.length;
  }
}

