C
C File:   argb2ci.f
C Author: brownrig
C
C Given a color index, returns:
C   i)  index of closest color in color table if index is an argb value
C   ii) the given index
C
C Created on January 11, 2012, 9:27 AM
C

      integer function argb2ci(index)
      implicit none
      include 'g01wsl.h'

      integer index, nearest
      integer i
      integer ARGBMASK, RMASK, GMASK, BMASK
      parameter (ARGBMASK = INT(Z'40000000',kind=4))
      parameter (RMASK     = INT(Z'00FF0000',kind=4))
      parameter (GMASK     = INT(Z'0000FF00',kind=4))
      parameter (BMASK     = INT(Z'000000FF',kind=4))
      real r, g, b, dist, mindist

      if (iand(index, ARGBMASK).eq.0) then
          argb2ci = index
          return
      end if

C     find the closest match, based upon distance in color space
      mindist = 2e31
      nearest = 0
      do i=1,mol
          r = (iand(index, RMASK) / INT(Z'0000FFFF',kind=4)) / 255.
          g = (iand(index, GMASK) / INT(Z'000000FF',kind=4)) / 255.
          b = (iand(index, BMASK))               / 255.

C         we don't need absolute distance, so forego the sqrt operation...
          dist = (r-sred(i))**2 + (g-sgreen(i))**2 + (b-sblue(i))**2
          if (dist .lt. mindist) then
              mindist = dist
              nearest = mcoli(i)
          end if
      end do

      argb2ci = nearest
      return
      end


